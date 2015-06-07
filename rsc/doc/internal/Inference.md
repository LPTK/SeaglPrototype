
# Advanced Considerations on Type Inference


## Unification

After typing the body of a definition, we end up with a bunch of constraints on a bunch of types including free type variables that will need to be generalized.

There are several things to do to simplify the final generalized type of the definition:
 * If we have a concrete type C applied to some types A.., and this concrete type is involved in a subtyping constraint with some `R`, we can:
 	* unify `R` with it if `R` is abstract (`R <: C A..` or `C A.. <: R` gives `C A..`)
 	* pairwaise unify the arguments `A..` and `B...` if `R = C B...`, acording to their variance (and if invariant, introduce a new type variable `R'` to be the LB or HB -- also applies to case above)
 	* report an error if `R = D B...` and `D != C`, `D` concrete
 	* do nothing otherwise (`R = D B..` and `D` is abstract)
 * If a type variable `R` is used only in covariant position, it can be substituted with the lower bound of its constraints
 * If a type variable `R` is used only in contravariant position, it can be substituted with the higher bound of its constraints
 * Simplify subtyping expressions


**Example**:

```
id x = x
id: 'X -> 'X

foo a = id a
foo: 'A -> 'X  // new type variable X comes from instantiating 'id'
where
	'A <: 'X             // because A is used as arg in fun of type X -> X
```

Here, `'X` is only used in covariant position (the function output), so we can replace it with its lower bound `'A`, and we obtain:
```
foo: 'A -> 'A
```

Note: Similarly, we could have replaced `'A` with `'X`, because `'A` was only used in contravariant position.


**Other example**:

Assume here that `"ok"` is of type `Str.Ref @static` (the closest to how C views strings).

```
unif (x: 'T) (y: 'T) = ()
unif: 'T -> 'T -> ()

foo a = unif a "ok"; a
foo: 'A -> 'A
where
	'A              <: 'T
	Str.Ref @static <: 'T
```
Unifying `'T` with `Ref` concrete type:
```
foo: 'A -> 'A
where
	'A      <: Str.Ref 'R
	@static <: 'R
```
Unifying `'A` with `Ref` concrete type:
```
foo: Str.Ref 'R2 -> Str.Ref 'R2
where
	'R2     <: 'R
	@static <: 'R
```
`'R` is used only in covariant position (in the subtyping expressions); replace it with its lower-bound `@static | 'R2`:
```
foo: 'R2 => Str.Ref 'R2 -> Str.Ref 'R2
where
	'R2 <: @static | 'R2
```
Simplifying the subtyping constraint, we get:
```
foo: 'R2 => Str.Ref 'R2 -> Str.Ref 'R2
where
	'R2 <: @static
```

Note: if unit `()` had been returned instead of `a`, we could have simplified foo's signature further to:  
```
foo: Str.Ref @static -> ()
```



**Other example**:

```
foo() = Nil : List _  // '_' denotes a type left to infere
foo: () -> List 'T
```
Because `'T` is only used covariantly (`List 'T` is covariant in `'T`), this signature can be simplified to:
```
foo: () -> List Nothing  // Nothing is the "Bottom" of the type lattice
```

EDIT: This example is probably not well-chosen; in Seagl, types need well-defined sizes, and the size of `List T` depends on the size of `T` (unless it is stored externally, which should not be the case for lists). So there is no actual well-defined  `List Nothing` type. However, the reasoning still holds when applied to things like regions and erased types (which are all the same size since allocated externally), where we truly have subtyping. The bottom element for regions is the empty region, but there is no top (but maybe we could introduce one).


### Note: Particular Case of First-Order Values

For values that are not functions, since value world is strict, we can't leave it with a higher-order type, so unless we can simplify the type to remove all free variables (like in `foo` above), we are left with an *unconstrained* type, that may be unified in the future with no more than one type.

OCaml does a similar thing:  
Since `list` is covariant, we can keep the polymorphic definition `foo` with generic type `'a`, even though `foo` is not a function:
```
# let foo() = [];; 
val foo : unit -> 'a list = <fun>
# let foo = [];;  
val foo : 'a list = []
```
However, in the following, since `ref` is invariant, when `foo` is not a function, we need to specialize `'a`, but since it is yet unconstrained OCaml uses an unknown type expression `'_a`.
```
# let foo(): 'a list ref = ref [];;
val foo : unit -> 'a list ref = <fun>
# let foo: 'a list ref = ref [];;  
val foo : '_a list ref = {contents = []}  // Notice the '_a
# foo := [1;2;3];;
- : unit = ()
# foo;;
- : int list ref = {contents = [1; 2; 3]}  // '_a magically transformed to int!
```

A proper representation for "some type that is not yet determined, and is yet unconstrained" would be an existential.



## First-Class Polymorphism

It seems that operation types significantly mitigate the need for first-class polymorphism. For example:
```
foo f = (f 0, f "ok")
foo: 'F -> ('F.app Int, 'F.app Str)

id x = x
id: 'X -> 'X
foo id: (Int, Str)

foo(x => x): (Int, Str)  // does this really work? is abstraction generalized as a subexpression? <- at least not in traditional HM (why not?)
```

However, I expect that it will be useful to be have actual first-class polymorphic functions (although I have a hard time finding any good example).
To define them, we use explicit type quantification (ie: type lambdas in value world)

```
foo f = (app f 0, app f "ok")  // error: cannot unify Int(0) and Str("ok") -- `app` forces interpretation as function types

foo = (f: X => X -> X) => (f 0, f "ok")
foo: (X => X -> X) -> (Int, Str)
```

Here is a better example. The following function is not typeable using let-polymorphism alone:
```
register x =
| (a,b) => register a; register b
| _ => log(toString x)
```


say we assume let-polym by default
can provide only partial specs for where fcp is needed/actually occurs



```
register[T: (_:Show, _:Show) | (_:Show)] (x: T) =
| (a,b) => register a; register b
| _ => log(toString x)

register: T ('A: Show, 'B: Show) | ('C: Show) => 
```

Notice that we did not give a return type 

```
Reg = RegBase | (Reg, Reg)
RegBase = 

register: ('X: ) -> ()
```






## Miscellaneous Niceties

Bounds on types can be added on the fly:
```
foo (x: 'T: Show) = print x.show
// or
foo (x: _: Show) = print x.show
// or
foo (x::Show) = print x.show  // not sure will keep this syntax, :: is useful for scope access syntax
```

There is always the less convenient but sometimes required:
```
foo (x: 'T) = print x.show
where 'T: Show
```

It is possible to define types that contain free type variables that will be unified later:
```
Ref T = T.RefTo 'R
// or
Ref T = T.RefTo _
```

It is possilbe to omit type parameters when ascribing a value:
```
foo (ls: List.List) = ls.flatten
foo: (List 'T).List -> List 'T
```

(I don't think we're really losing anything by allowing this shortcut.)





