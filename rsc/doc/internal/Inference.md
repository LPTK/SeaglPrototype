
# Advanced Considerations on Type Inference


## Unification

After typing the body of a definition, we end up with a bunch of constraints on a bunch of types including free type variables that will need to be generalized.

There are several things to do to simplify the final generalized type of the definition:
 * If we have a concrete type C applied to some types A.., and this concrete type is involved in a subtyping constraint with some `R`, we can:
 	* unify `R` with it if `R` is abstract (`R <: C A..` or `C A.. <: R` gives `C A..`)
 	* pairwaise unify the arguments `A..` and `B...` if `R = C B...`, acording to their variance (and if invariant, introduce a new type variable `R'` to be the LB or HB -- also applies to case above)
 	* report an error if `R = D B...` and `D != C`, `D` concrete
 	* do nothing otherwise (`R = D B..` and `D` is abstract)
 * If a type variable `R` is used only in covariant position, it can be substituted with the least upper bound (LUB) of its lower bound constraints
 * If a type variable `R` is used only in contravariant position, it can be substituted with the greatest lower bound (GLB) of its upper bound constraints
 * Simplify subtyping expressions


**Example**:

```
// Assume:
id x = x
id: 'X -> 'X

foo a = id a
foo: 'A -> 'X  // new type variable X comes from instantiating 'id'
where
	'A <: 'X   // because A is used as arg in fun of type X -> X
```

Here, `'X` is only used in covariant position (the function output), so we can replace it with its lower bound `'A`, and we obtain:
```
foo: 'A -> 'A
```

Note: Similarly, we could have replaced `'A` with `'X`, because `'A` was only used in contravariant position.


**Other example**:

Assume here that `"ok"` is of type `Str.Ref @static` (the closest to how C views strings).

```
// Assume:
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
foo: Str.Ref 'S -> Str.Ref 'S
where
	'S      <: 'R
	@static <: 'R
```
`'R` is used only in covariant position (in the subtyping expressions); replace it with the LUB of its lower bounds,  
`@static | 'S`:
```
foo: Str.Ref 'S -> Str.Ref 'S
where 'S <: @static | 'S
```
Simplifying the subtyping constraint, we get:
```
foo: Str.Ref 'S -> Str.Ref 'S
where 'S <: @static
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

**EDIT**: This example is probably not well-chosen; in Seagl, types need well-defined sizes, and the size of `List T` depends on the size of `T` (unless it is stored externally, which should not be the case for lists). So there is no actual well-defined  `List Nothing` type. However, the reasoning still holds when applied to things like regions and erased types (which are all the same size since allocated externally), where we truly have subtyping. The bottom element for regions is the empty region, but there is no top (but maybe we could introduce one).

**Note**: It could be interesting to define a bottom for every type lattice, namely `No: * -> *`. For example, we have `No Int <: 42, `No Int <: 666, `No Int <: Int`; it has the mem repr of an `Int`, but it is an "impossible" value that subtypes every subtype of Int.  
It would naturally come up in the signature of things like:
```
bar () = if bar() then bar() else bar()
bar: () -> 'R  where 'R <: Bool
```
Type `'R` has a `Bool` constraint so it must adopt the same memory represenation (it must be in the `Bool` lattice), so we add the implicit bound `R :> No Bool`, which is its only lower bound and it is covariant, so we can replacing it with `No Bool`:
```
bar: () -> No Bool
```

So, in the example above, we could have had:
```
foo: () -> List (No 'U)
```
Although it does not really simplify the type expression, so we should probably just stick to `() -> List 'T`.



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

foo(x => x): (Int, Str)  // unification failure
```
The last example could be made to work if our HM generalizes abstractions as if they were let-bound (which is useless to do in standard HM because there is no first-class polymorphism nor operation types).

However, I expect that it may still be useful to have actual first-class polymorphic functions (although I have a hard time finding any good example).
To define them, we use explicit type quantification (ie: type lambdas in value world)

```
foo f = (app f 0, app f "ok")  // error: cannot unify Int(0) and Str("ok")
                               // -- `app f` forces interpretation of `f` as a function type

foo = (f: X => X -> X) => (f 0, f "ok")
foo: (X => X -> X) -> (Int, Str)
```

If we want the passed function to have effects (which would give some sense to the example), we have to add it to the argument function's type (and it is sufficient to have let-polymorphism for the effect even if the passed function will have dependent effects; the effect type used will be the unification of those):
```
foo = (f: X => X -> X ! _) => (f 0, f "ok")
foo: (X => X -> X ! 'E) -> (Int, Str) ! 'E
```

**Note**: even the following work:
```
applyAll fls = fls .map (f => (f 0, f "ok"))
applyAll: 'Fls -> 'Fls .map ('F -> 'F.app Int, 'F.app Str)
// or
applyAll (fls: 'F.List) = fls .map (f => (f 0, f "ok"))
applyAll: 'F.List -> ('F.app Int, 'F.app Str).List

applyAll id                // ok, has type (Int,Str).List
applyAll(x => x)           // unification error (unless we generalize lambdas)
applyAll(X => (x:X) => x)  // ok (uses first-class polymorphism)
```
Indeed, `(X => (x:X) => x)` has first-class polymorphic type `X => X -> X`.

The following fails, because it tries to require fls to contain functions all of the same type `'A -> 'B`, whereas it should contain first-class polymorphic functions:
```
applyAll (fls: ('A -> 'B).List) = fls .map (f => (f 0, f "ok")) // unification error
```

So, first-class polymorphism may be useful, but is still not necessary in most cases.




## Miscellaneous Niceties

Concept bounds on types can be added on the fly:
```
foo (x: 'T: Show) = print x.show
// or
foo (x: _: Show) = print x.show
// or
foo (x::Show) = print x.show  // not sure will keep this syntax, :: is useful for scope access syntax

foo: 'T -> ()  where 'T: Show
```

There is always the less convenient--but sometimes required--explicit `where` clause:
```
foo (x: 'T) = print x.show
where 'T: Show
```

It is possible to define "floating"  types, that contain free type variables that will be unified later:
```
// Assuming we have RefTo: * -> @ -> *
Ref T = T.RefTo 'R
// or
Ref T = T.RefTo _
```

It is possilbe to omit type parameters when ascribing a value:
```
foo (ls: List.List) = ls.flatten
foo: (List 'T).List -> List 'T
```

(I don't think we're really losing anything by allowing this shortcut; there is almost no case where you'd actually want to type a value with the first-class polymorphic type `T => List T`.)





