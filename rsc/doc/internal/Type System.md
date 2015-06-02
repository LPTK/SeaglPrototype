
# Type and Effect System Prototype Ideas

_Version: 0.1_


## Introduction

Goals:
 - flexible, algebraic
 - advanced inference
 - support for ad-hoc polymorphism
 - abstraction allowing raw performance of generated code, without compromise
 - metaprogramming
	


## Syntax and Basic Semantics

### Type and Value worlds

Types have the same syntax as values. We refer to elements of this shared syntax as "terms".
They are usually distinguished by their name (types begin with a capital letter).

````
A = X  // type
a = x  // value
````

Wrongful mixes of type and value expressions will be detected after parsing, during AST construction.

Just like values, higher-order types can be expressed in the form of lambdas. This is how generic types are expressed.
The following are equivalent:
````
List = T => (Cons T | Nil)
List T = Cons T | Nil
````

The language of types is pure and lazy (allowing the definition of recursive types), while the language of values is strict and effectful.

### Uniform Function Call Syntax

Both type and value worlds embrace a functional interpretation of the *Uniform Function Call Syntax*, under which `a.b c` is basically a synonym of `b a c`. For example, `2.add 2 == add 2 2` and `Ref Int Src == Int.Ref Src`.

In value world, there is a semantic difference between calls in prefix and infix forms: a prefix call will first look into the current scope for a free-standing function, and will reject any possible ambiguity, while a call in infix form will first look for a method in the type of the receiver before looking for a free-standing function. If several `.add` methods are in scope, a call to `add a b` will be ambiguous and rejected, while a call to `a.add b` will dispatch according to the type of `a`.  
This allows a form of ad-hoc polymorphism, and gives some freedom with respect to inference. See section *Abstract Operation Types* and *Inference* for details.


### Builtin Type Operators

Alternation `A | B` and tuple `A, B` are defined structurally: `A | B | C == A | (B | C)`



## Abstraction


### Partial Evaluation

One fundamental property of the system is that type expressions are partially evaluated and unified as much as possible, up to unknown abstract types. The system only aggregates constraints about abstract types and makes them part of the object's signature.

In order to branch on the structure of a type, a type lambda will require the `Info` bound on this type. Evaluations of such lambdas will block until the type is unified with a concrete type.


### Abstract Operation Types

Applying types to a value operation defines *operation types*. For example, while `A.Foo B` is just a shortcut for `Foo A B`, `A.foo B` is a special type that represents *the type of calling `foo` on an object of type `A`, passing it an object of type `B`*.  
When `A` is concrete, it will be immediately reduced (evaluated) to the corresponding type. However, when `A` is abstract, it will be kept as is until we can evaluate it at a later stage.



### Structural Types and Bounds

Structural types are abstract types that can be use as type bounds, writen `Type : Bound`.

```
MyStrcType = { mtd: Self -> Int; free: Int -> Int }

test [T: MyStrcType] (x: T) = x.mtd + (T::free 42)
// or
test (x: T: MyStrcType) = x.mtd + (T::free 42)

test: (T: MyStrcType) => T -> Int
```


### Strict and Liberal Implementation

[TODO]

Type `A` implements `B`, written `A <:< B` or `A: B` if it provides the required definitions.

Implementation is modular and extensible (ie: sort of like modular type classes).

We have things like `Union(Int,Str): Int | Str`, `Union(Int,Str): Str | Int`, `Union(Int, Str.Ref Src): Int | Str`, etc.  
`Union` is a C-like tagged union.


### Classes

[TODO]

```
MyClass = class(x: Int; y: Str)
mc = MyClass(42, "ok")
r = mc.y
r: Str.Ref @mc.y
```


### Pattern-Matching

[TODO]

Matching `Foo` looks in the current scope for a definition named `ext_Foo` ...




## Effects

### Regions

Regions are particular types. With each local value `v`, a region `@v` is implicitly defined.


### Abstract and Concrete Effects

Effects are associated with values, embedded into their type. Type `T ! S` means "the type `T` along with the effects associated with type `S`".

When the particular effects of a type are known, they can be inlined and will be of the form `<R,I>`, where `R` is the region that the expression reads and `I` is the region that it invalidated.


### References and Copy Types

Referencing a local value or accessing an object field will usually return a reference to this object. Some types, like `Int` and `Ref T Src` are *copy types*, which means that accessing them will simply yield copies, not references (therefore, we cannot manipulate reference-to-reference types, but there are alternatives to achieve the same effect).




## Inference

### Basics

```
foo f x = f x
foo: F,X => F.apply X  // Operation type on abstract types F and X

id x = x
id: X => X -> X

str: Str = "ok"
(str) : Str.Ref @str    // referencing a value of non-copy type takes a reference to this value
id str : Str.Ref @str
foo id str : Str.Ref @str

op = s => s + "!"
op: S => S + Str
assert (foo op str : Str) == "ok!"
opStr = op[Str]           // specialization of a higher-order typed value
opStr: Str -> Str
opStr2 = op : Str -> Str  // specialization by unification of the applied type schema
opStr2: Str -> Str
```

### Unification

Keyword `where` expresses constraints on abstract types.

```
bar f x y = if x < y then f x else f y
bar: F,X,Y,R => F -> X -> Y -> R  ! X < Y ! F.apply X ! F.apply Y
where
	F.apply X <: R
	F.apply Y <: R
	X < Y <: Bool

op (n:Int) = n+1
op: Int -> Int

bar op 42 "ko"  // Error: 'op' not defined on Str; '<' not defined on Int Str
assert (bar op 42 24) == 25
assert (op.bar 42 24) == 25
```

### Example with Effects

```
foo f = f 42; s = "ok"; f ok; ()
foo: F => F -> ()  ! F.apply Int ! F.apply (Str.Ref @local)

i = 0
i: Int
op = _ => i += 1
op: T => () ! <@i, @i.const>  // op reads i, mutates i (op invalidates i's "constness")
foo op : () ! <@i, @i.const>
```


### Regular Types

[This is still mostly an open question; to refine]

Regions can be inferred and we can compute inclusion and intersection on them (cf: first semester project report) because they are as expressive as regular expressions.  
Function `get` on a list `ls: List Str` will return a `Str.Ref @ls.tail*.head`.

The question is: could this somehow be generalized to all types? Have inference approximate to some kind of regular types in general?  
Note that types like `Tree` are not "regular", under most definitions. It is not very clear how inference would work in the grand scheme of things.

But at least, we can infer region bounds.

```
map[T] (ls: List T) f = ls.match {
| Cons(h,t) => Cons(f h, t.map f)
| Nil => Nil
}
map: T,F => (ls: List T) -> (f: F) -> List (F.apply (T.Ref @ls.tail*.head))
// or, more likely:
map: T,F => (ls: List T) -> (f: F) -> Cons(F.apply (T.Ref @ls.tail*.head)) | Nil
```

The type of `map ls f` is a new list which elements have the type of applying `f` on elements of the original list.

```
ls = List("aa","bb","cc")
ls: Str.List
r = ls.map id
r: (Str.Ref @ls.tail*.head).List
s = r.head
s: Str.Ref @ls.tail*.head
```



## Erasure and Existentials

[TODO]



## Subtyping

Contrary to many languages, subtyping is not used in conjunction to some kind of inheritance mechanism. It is essentially restricted to regions, erased types, and variant generic types (for example, `List A <: List B` if `A <: B`).  
In fact, for efficiency reasons, a type `A` cannot subtype `B` if they don't have compatible memory repreentations (size and alignment) -- but this is an implementation detail.


### Regions Sutyping

Obviously, `@a.b` <: `@a.(b|c)`, etc.


### Erased Types Sutyping

If abstract type `T` implements `S`, then an object of erased type `?T` can be used as an `?S`.




## Metaprogramming

Using type lambdas, computation at compile-time becomes trivial...

Formally introduce the dual syntax `[.]` and its uses...

Macros...





