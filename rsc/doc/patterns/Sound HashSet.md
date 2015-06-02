
# Sound `HashSet` Pattern

### Motivation

Allow the definition of a `HashSet` generic in the hash function, so that it is guaranteed to always hash elements the same way.


### Implementation

The idea is to make the `HashSet` dependent on "*the constness of what the hash function reads when given elements of the set*". All these concepts should be expressible in Seagl.

What it would look like:
```
HashSet (T: Move) (H: T -> Int) = class(size: Int; elems: T.Buffer.Array[size]; hash: H) {
	alias @content = @elems.content.content
	depend ReadsOf(H.apply (T.Ref @content)).const  // explicitly state the dependency
	
	add (self: Self) (x:T) = ...
}

MyClass = class(n: Int)
m = 123
do {
	hs = mkHashSet[MyClass](mc => mc.n + m)
	hs.add MyClass(42)
	mc = hs.iter.next
	mc: MyClass.Ref @hs.content
	mc.n <- 0 // ILLEGAL: would invalidate hs
	m <- 0    // ILLEGAL: would invalidate hs
}
m <- 0 // ok, hs is dead at this point
```



