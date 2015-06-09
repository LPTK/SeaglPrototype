# Simple Sum Types

## Principle

There are many ways to approach sum types, and I have developed a few. Some were based on concepts and parametric polymorphism, so that monomorphization would ensure efficiency, but it felt unnecessarily complicated. Achieving efficiency through optimization passes that specially know about sum types seems more appropriate, in order to stick to an uncluttered language design.

In this simpler approach, we equate sum types and tagged unions. But the sum of two references is not the same as a reference to a sum, although the latter (more restricted) subtypes the former.


## Subtyping

Type `A|B` is a tagged union of types `A` and `B` (its size is the maximum of the size of `A` and `B`), but also subtypes `A.Ref|B` (and thus `A.Ref|B.Ref` by commutativity).

Sum types are distributive on `Ref`, so that `(A|B).Ref 'R <: (A.Ref 'R)|(B.Ref 'R)`.

We also have `A <: A|B`, which means that we can coerce any value of type `A` to a value of type `A|B`, for any `B`. This will sometimes entail a *move* of the `A` value, so in the special case when `A` is immovable, this coercion might not be possible.


## Pattern-Matching

Assume the following usual definition of `List`:
```
Nil    = object
Cons T = class(head: T; tail: T.List.Ext)
List T = Cons T | Nil
```
(`Ext` marks an externally allocated value, without which the memory layout of `Cons` would be ill-defined.)

**Note**: A more efficient `List` representation in the case of big stored values would move the `Ext` to `List`, as in `List T = T.Cons.Ext | Nil`, so that the `List` object remains small in case it is nil.

These definitions automatically introduce constructors and extractors in value world. For example we have:
```
Cons.apply:   ('T, 'T.List.Ext) ->  'T.Cons
Cons.unapply:  'T.Cons.Ref 'R   -> ('T.Ref 'R.head, 'T.List.Ext.Ref 'R.tail)
```
Notice that the extractor exposes references to the values contained in the inspected `Cons.Ref` argument. This avoids unnecessary copy/moves. When matching an object, we always want references to values that live in the object, not new values (unless they are of copy types, in which case we'll get values).

Consequently, expression `(Cons(h,t) => h)` has type `'T.Cons.Ref 'R -> 'T.Ref 'R.head` (give me a `Cons` reference, and I give you a reference to its head).

As expected, matching several objects of different shape will infer, for the inspected object, a type that is the sum of the matched types.
```
foo x = x .match {
|	Cons(h,t) => 0
|	Nil       => 0
}
foo: 'T.Cons.Ref 'R | 'Nil.Ref 'S -> 0
```

Notice that this type is not equivalent to `'T.List.Ref 'R -> 0`, because in the latter, we have a reference to a tagged union of `Cons` and `Nil`, but in the former we have a tagged union of references to objects of these types, that can live at different places and may not be contained in one tagged union!  
Since sum types are distributive on `Ref`, we can pass this `foo` function a reference to a local value, of type, e.g.: `Int.List.Ref @ls`.

In order to make things smoother, special extractors could be defined (possibly with the same names, shadowing the automatically-generated ones) so that they require a reference to a list, which is more restrictive but may give better-looking types. For example, a vararg `List(xs..)` and a prepension `h :. t` extractors:

```
size ls = ls .match {
|	h :. t  =>  1 + (size t)
|	List()  =>  0
}
size: 'T.List.Ref 'R -> Int
// or
size: _.List.Ref _ -> Int
```

As seen above, in order to simplify type expressions, we collapse types that live in the same lattice, taking their *LUB* both when on the left-hand side and on the right-hand side of the function.  
Here, `Int+Int == Int` and the *LUB* of `Int` and `0` is `Int`, so we return `Int`.

**Note**:  Not completely sure about doing that for the lhs: it means matching `0` and `1` will yield a type saying we're matching `Int`, so the match may fail and we have to emit a non-exhaustive match warning. But there may be good cases where we really only want to match a few values and expose that in the type signature.  
As for the rhs, in the example above, the sum type `Int|0` would have been reducted to `Int` anyway (or would it? -- see below).


## Simplification

It might be possible to simplify sum types by collapsing same-lattice types, but this is to be carefully considered, since it might not work well in the context of generic code, where the concrete types are not known, and thus not simplified, until they are exposed later.


## Implementation

Sum type `A|B` can be viewed as a type from which we either can "move out" an `A` or a `B`. Efficient memory representation for the union will depend on the types in the sum and their sizes.

Notice that moving a value invalidates the source object, unless the object is of copy type (like `Ref` or `Int`). Since most pattern-matching should happen on references rather than values directly, this *move* semantics will not usually be visible or a problem.  
However, it means that it will sometimes be necessary to explicitly match a reference, not a value, as in:
```
foo mc = mc.match { mc: MyClass => mc.n | n: Int => n }
// ^ will invalidate and destruct any MyClass object passed to this function!
foo: MyClass | Int => Int // notice MyClass is passed by value (moved)

foo mc = mc.match { mc: MyClass.Ref => mc.n | n: Int => n }
foo: MyClass.Ref _ | Int => Int  // ok, passed by reference, no invalidation
```


