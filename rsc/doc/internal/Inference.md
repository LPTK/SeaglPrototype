
# Advanced Considerations on Type Inference


## Unification

After typing the body of a definition, we end up with a bunch of constraints on a bunch of types including free type variables that will need to be generalized.

There are several things to do to simplify the final generalized type of the definition:
 * If we have a concrete type C applied to some types A.., and this concrete type is involved in a subtyping constraint with some `R`, we can:
 	* unify `R` with it if `R` is abstract (`R <: C A..` or `C A.. <: R` gives `C A..`)
 	* pairwaise unify the arguments `A..` and `B...` if `R = C B...`, acording to their variance (and if invariant, introduce a new type variable `R'` to be the LB or HB)
 	* report an error if `R = D B...` and `D != C`, `D` concrete
 	* do nothing otherwise (`R = D B..` and `D` is abstract)
 * If a type variable `R` is used only in covariant position, it can be substituted with the lower bound of its constraints
 * If a type variable `R` is used only in contravariant position, it can be substituted with the higher bound of its constraints

Example:
```
foo() = Nil : List _  // '_' denotes a type left to infere
foo: T => () -> List T
```
Because `T` is only used covariantly (`List T` is covariant in `T`), this signature can be simplified to:
```
foo: () -> List Nothing  // Nothing is the "Bottom" of the type lattice
```

EDIT: This example is probably not well-chosen; in Seagl, types need well-defined sizes, and the size of `List T` depends on the size of `T` (unless it is stored externally, which should not be the case for lists). So there is no actual well-defined  `List Nothing` type. However, the reasoning still holds when applied to things like regions and erased types (which are all the same size since allocated externally), where we truly have subtyping. The bottom element for regions is the empty region, but there is no top (but maybe we could introduce one).


### Particular Case of First-Order Values

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





