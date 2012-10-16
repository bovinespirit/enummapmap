EnumMapMap
==========

Based on `containers 5.0`, EnumMapMap provides an EnumMap, similar to IntMap but
accepting any Enum type.  It also provides an EnumMap of EnumMaps type.  The Key
is built using an infix operator (:&) and a base type, K.  A typical use might
look like this:

```haskell
import           EnumMapMap.Strict (EnumMapMap, (:&)(..), K(..)
import qualified EnumMapMap.Strict as EMM

data Apple = Apple {weight :: Int, Circumference :: Int}
newtype AppleID = AppleID Int
newtype TreeID = TreeID Int
newtype OrchardID = OrchardID Int

type Orchards = EnumMapMap (OrchardID :& TreeID :& K AppleID) Apple

o = OrchardID 3
t = TreeID 34
a = AppleID 1
apple = EMM.lookup (o :& t :& K a) orchards
```

If it is being used as a single EnumMap then the Key must have type K.

```haskell
type IntMap v = EnumMapMap (K Int) v

im :: IntMap String
im = EMM.singleton (K 3) "Three"
im' = EMM.insert (K 5) "Five" im
```

The code ensures that only the root EnumMap can be empty.  There are lazy and
strict variants.  Both are strict in the keys, but the strict version is strict
on values as well.

The keys can be split to allow operations on whole subtrees. The level of the
split is defined by a type.  These types range from `d1` to `d10`.

```haskell

tree = EMM.lookup (o &: K t) $ splitKey EMM.d1 orchards
newOrchards = joinKey $ delete (K o) $ splitKey EMM.d2 orchards
```

In the previous example `unsafeJoinKey` could be used instead of `joinKey` as
there is no possibility of an empty subtree being left.

TODO:

- More documentation
- More tests
- EnumMapMap & EnumMapSet - elemKeys, intersectBy
- Check that Strict really is strict and Lazy really is lazy.
- More functions - mapMaybe, update, mergeWithKey,  foldr'
- Optimisation
- Benchmarks
- Replace d1..d10 with numbers

