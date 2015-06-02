
# Subtyping constraints vs sum-type polymorphism

### Comparison

**Example**:
```
foo (b: Bool) = if b then 42 else 24

// Subtyping:
foo: R => Bool -> R
where
	42 <: R
	24 <: R

// Parametricity:
foo: R => Bool -> (42 | 24 | R)
```

In any case, we can use result of `foo` anywhere, eg where an `Int` is expected, or even as a refined `Int{_ > 0}`.  
In general, it won't lose the free type `R` until it needs to be unified


**Other example**:
```
get (ls: List _) = if ... then ls.head else ls.tail.head

// Subtyping:
get: T,R => List T -> T.Ref R   // The types in each branch have been unified, cf: concrete type Ref
where
	@ls.head      <: R
	@ls.tail.head <: R

// Parametricity:
get: T,R => List T -> T.Ref (@ls.head | @ls.tail.head | R)
// ie
get: T,R => List T -> T.Ref (@ls.(head | tail.head) | R)
// ie
get: T,R => List T -> T.Ref (@ls.(tail | $).head) | R)
```


The main difference between these approaches
 - subtyping approach needs variance knowledge about type parameters
 - parametric approach needs even field accessors to expose a free type in order to use the ref as a larger ref...  
	if the user specifies the too specific non-parametric type directly, it will be a problem!  
	in a way, this approach is akin (but not actually similar) to putting bounds in type args instead of relying on variance


