# Basic Syntax – Incremental Definition

The idea is to quickly have a simple syntax that allows us to write nice things. We'll see later for syntax sugar and advanced features.

## Tentative State

### Iteration 1

Be able to "let"-bind both value and type names to their definitions, and stream them. Type lets should be recognized by the name that starts with a capital letter.

```
foo = bar x y; T = List Int; print 42; ()
```

*AST question*: Blocks? They may make it easier to represent this mix of type and value declarations...


### Iteration *n* (future)

**Have a whitespace-significant syntax.**

 * Parse symbols (compositions of non-alhphanumeric, non-space characters) and methods (dot followed without space by alphanums) as operators

 * Allow to split a line on `=` or on an operator and interpret the following indented lines as a block
```
foo(42,align="left",visible=true)
// or
foo
	42
	align = "left"
	visible =
		true

x.foo y
// or
x.foo
	y
// but not:
x.foo
y      // this is a different statement
```

* View lines starting with `|` as part of a pattern-matching closure
```
foo =
| 0 => "null"
| _ => "non-null"

// ie:

foo = { 0 => "null" | _ => "non-null" }
```


#### Experimental: more whitespace significance

It's easy to distinguish a space from no space. All languages are already whitespace-significant in this respect. But we could take it further to make the syntax more powerful.

`foo f(x)` (ie `foo(f(x))`) has a different meaning than `foo f (x)`, the latter being equivalent to `(foo f) x`, as in ML.

Similarly:
```
f(x).y     ==  (f x).y
f (x).y    ==  f (x.y)

f x+y      ==  f(x+y)
f x +y     ==  (f x) + y
f x + y    ==  ((f x) +) y  // ? (probably not)

f a.x      ==  f(a.x)
f a .x     ==  (f a).x      // ?

f 1,2,3    ==  f(1,2,3)
f 1, 2, 3  ==  f 1, 2, 3
```



