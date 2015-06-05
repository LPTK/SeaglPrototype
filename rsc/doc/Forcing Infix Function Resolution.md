
### Infix Function Inference

Functions called in infix position on abstract types will usually be left uninterpreted, and operation types will be inferred for the result of such calls.  
By contrast, prefix notation will always search for definitions in the current scope in order to be typed, and require to be non-ambiguous (overloading should be locally unambiguous).

For example (operator `:.` is list prepension -- see note at the bottom):

```
import List

foo ls = 42 :. ls
foo: L => Int :. L  // operation type

bar ls = (:.) 42 ls
bar: A => List A -> List A
where
	Int <: A
```
Or, assuming `Int` doesn't have any super type -- although it does have subtypes:
```
bar: List Int -> List Int
```

But arguably, epxression `42` should have type `42`, so we should really have:
```
bar: List A -> List A
where
	42 <: A
```


### Forcing Resolution

It is possible to ask, within any scope, to use a set of infix operators as if they were prefix. This requires no ambiguity (no overloading possible). It is done with the `use` directive.

```
import List

use :.

foo ls = 42 :. ls
foo: A :> 42 => List A -> List A
```



### Notes on Right Associativity

Methods can be made left or right associative depending on which side we write the dot:

```
ls .push x .push y  ==  y push. x push. ls
```

On the other hand, operators are always considered to be left-associative, unless they end with a dot, in which case the dot can also be used in the front to make them left-associative.

```
1 :. 2 :. Nil  ==  Nil .: 2 .: 1
```

We could also name commutative methods with dots on both side: `2.add.2 == 2.add 2 == 2 add.2`

**Ambiguity**

What is the meaning of `1 push. ls .push y` ?  
We can probably just assume left-to-write precedence by default: `(1 push. ls) .push y`






