# Identifier Types

## Introduction

An identifier type/value is an identifier preceded by a backtick`` ` ``.

Identifiers are used to denote types which don't embed any other information than their name. For example, here is the definition for type `Bool`:

```
Bool = `True | `False
```

Identifiers are also used to emulate type constructors, and a simplified syntax makes it look seamless.`` `A X Y Z`` is equivalent to ``(`A,X,Y,Z)``

Example:
```
Option T = `None | `Some T

// ie:
Option T = `None | (`Some, T)

si: Int.Option = `Some 42
```





### Better Flags and Enumerations

Sometimes, simple basic types have more than two possible values; or there are two values, but no obvious `true` and `false`, as in
`` `Dynamic | `Static `` instead of a `Bool` which we would name `isStatic` or `isDynamic`.



### Polymorphic Variants

We can encode Ocaml's [Polymorphic Variants](https://realworldocaml.org/v1/en/html/variants.html#polymorphic-variants) by using sum types, identifiers and pairs.

The idea to express ``[`A of int | `B of string]`` is `` `A Int | `B Str``, i.e.: ``(`A, Int) | (`B, Str)``.

In OCaml:
```
let basic_color_to_int = function
| `Red   -> 1
| `Green -> 2
| `Blue  -> 3

val basic_color_to_int : [< `Red | `Green | `Blue ] -> int

let color_to_int = function
    | `Basic (basic_color,weight) ->
      let base = match weight with `Bold -> 8 | `Regular -> 0 in
      base + basic_color_to_int basic_color
    | `Gray i -> 232 + i

val color_to_int : [< `Basic of [< `Red | `Green | `Blue ] * [< `Bold | `Regular ] | `Gray of int ] -> int
```

In Seagl:
```
basicColorToInt =
| `Red   => 1
| `Green => 2
| `Blue  => 3

basicColorToInt: `Red | `Green | `Blue -> 1 | 2 | 3

let colorToInt =
| `Basic basic_color weight =>
  base = weight.match { `Bold => 8 | `Regular => 0 }
  base + basicColorToInt basic_color
| `Gray i => 232 + i

colorToInt: `Basic (`Red | `Green | `Blue) (`Bold | `Regular) | `Gray Int -> Int
```

Or, if `+` is uninterpreted:

```
colorToInt: `Basic (`Red | `Green | `Blue) (`Bold | `Regular) | `Gray `A -> 1 | 2 | 3 | (Int + `A)
```











