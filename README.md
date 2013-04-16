Algebraic classes are type classes where all the methods return a value of the same type, which is also the class parameter.
Examples from `base` are `Num` and `Monoid`.

F-algebras are functions `f a -> a`, where the functor `f` is called the signature, and the type `a` the carrier.

This package relates these 2 concepts, and can create conversions between the two using Template Haskell.
More specifically, it can generate:

* signatures from algebraic classes
* instances of algebraic classes from F-algebras.

This is useful because type classes are more commonly used in Haskell than F-algebras, but F-algebras are
easier to work with, because they are just functions.