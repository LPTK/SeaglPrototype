<!--
langname	version
C**			0.1
MetaLL		0.2
EAGL		0.3
Seagl		0.4
-->

# Language Fixture, Proposition 0.4.1


## Functions

Syntax: `A -> B`

[TODO]

### Effects

[TODO]


### Refinements

Several kinds:
 - dependency: use some custom syntax
 - effects: use the `!` type-level function on return type
 - [future] contracts: use invariants on param or ret



## Tuples, Records & Abstract Records

Unlike some previous propositions, here these concepts are built-in.

They are all defined inductively.


### Tuples

Syntax: `(A, Rest)`

Where `A` and `Rest` are any types, and if `Rest` is of tuple type `(B, C)`, we can write the whole thing `(A, B, C)`.

Construction and extraction syntaxes are identical. 

Semantics: types stored by value, in most optimally compact way.  
Nota: this does require all RHS types to be known in order to compute offsets -- so offset cannot be prematurely computed with an incomplete uple type like `(Int, 'X)`.

Note: `Unit` is a particuar case, an empty tuple `()`.


### Records

Syntax: `(a: A, Rest)`

Where `a` is an identifier. `Rest` is either `Unit` (in which case the type can be written `(a: A)`) or a record type that extends the record.

Construction syntax: `(a = value, rest)`

Extraction syntax:
 - `(a: A;)` for 1-element records (writing just `(a: A)` will only extract a `A` bound to local identifier `a`)
 - `(a: A; rest)` otherwise, where `rest` is another record matcher.

Record and tuple syntax can be mixed, if all named arguments (the record part) are in the end, eg: `(1,2,x=3,y=4)`.


### Abstract Records

The goal is to provide an interface to access named elements without committing to a memory representation.

Syntax: `{a: A; Rest}` or `{a: A = value; Rest}`

Where all default-valued parameters are located in the end [useful restriction?].

I would say that there is no construction syntax! Use one of the above to instanciate an Abstract Record. They will be compatible via subtyping.

Extraction syntax: `{a: A; rest}` or `{a: A = value; rest}` (it allows default values).


#### Free Type Variables

Record extraction syntax and field access yields **reference types**, pointing to regions that are free type variables.

For example, `{n: Int; s: Str}` will extract `n: Int` (because `Int.Ref _ == Int`) and `s: Str.Ref 's`, where `'s` denotes the free region variable where `s` is assumed to live.


### Subtyping relations

Assume subsumption, and the usual element-wise (point-wise) subtyping relations.

Concrete tuples implement abstract tuples:  
`(a: A.Ref R; B) <: {a: A; B}` with `'a = R`

Order doesn't matter:  
`(a: A.Ref R; b: B.Ref R'; C) <: {b: B; a: A; C}` with `'a = R & 'b = R'`

Names can be ommitted:  
`(A.Ref R, B) <: {a: A; B}` with `'a = R`

Default-valued arguments need not be produced:  
`() <: {a: A = value; B}`

One value is as good as a one-element tuple/record:  
`A <: (A, ())     // does this tuple type even exist?`  
`A <: (a: A; ())  // the above would suffice...`

Value can be coerced to reference:  
`A <: A.Ref 'a`, where `'a` is a new locally-defined identifier for the temporary value



### As Function Parameters

Functions will either be curried (in particular, the self-parameter is always curried) or include an abstract record as the input type.
This allows for flexible named argument passing:
```
foo: {n: Int; s: Str; b: Bool = true} -> ()

foo(42)       // error: not enough arguments
foo(42,"ok")
foo(s = "ok", n = 42)
foo(42, b = true, s = "ok")
// etc.
```


#### Values Instead of References

Single values can be coerced to references, allowing things like:
```
print Person("John, 42").name   // `name` takes in a `Person.Ref _`, not a `Person`
r = ref Person("John, 42")      // r: Person.Ref 'tmp0
```

However, when trying to pass a value to an abstract record, it will not work. But then one can use the `ref` function, which is nothing else than a fancy identity.
```
ref: T.Ref 'r -> T.Ref 'r

foo: {n: Person} -> Int
foo Person("John", 42)        // error: got Person, expected {n: Person}
foo (ref Person("John", 42))  // ok
```


### As Function Returns

Functions with multiple return values should usually return tuples and records without fearing for performance.
We should always encode such functions in a way that no unnecessary copy/move are required (eg: pass a pointer to destination).

For example: `asReal: 'N.MyNumber.Ref _ -> (re: 'N; im: 'N)`.

We could even not require `Move` capability from parts that are returned and then immediately placed somewhere.


### Examples

In all cases, it is possible to extract these constructs by providing `_` as the second argument, thereby abstracting over the size of the rest of the construct:
```
sum    {x;y}   =  x+y       // only allows two elements to be passed in
sum:   {X;Y}   -> X+Y       // operation type
sumObj {x;y;_} =  sum(x,y)  // allows any record/tuple that contains more elements
sumObj:{X;Y;_} -> X+Y

sum(1,2) == 3  // ok
sum(1,2,3)     // error: found (Int,Int,Int), required {'X,'Y}
sumObj(1,2,3)  // ok
sumObj entity  // ok as long as `entity` provides x/y
```

Note that the same `sum` function can be written directly:
```
sum  a = a.x + a.y
sum: A -> A.X + A.Y  // composite operation type

// or, taking effects into account:
sum: A -> A.X + A.Y ! A.X ! A.Y
```


## Tagged Unions and Pattern Matching

Syntax: `A|B`

The beauty of these is that the information about the current tag can be stored (often with no space overhead) in the references, not the values, allowing great flexibility (subtyping, etc.).


### Subtyping Relations

```
A <: A|B
A <: B |- A|C <: B|C
A|B <: B|A
// etc.
```

Note that `A|B|C == A|(B|C) == (A|B)|C`.


### Pattern Matching

[TODO]


## Type Classes & Modules, Cakes

[TODO]



## Classes

with private data

not first class? use type macros

[TODO]


...

