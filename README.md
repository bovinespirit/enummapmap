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
on values as well.  The data structures are the same however the key types are
different for the strict and lazy versions so strict operations can be performed
on a lazy EnumMapMap.

_Subtrees_

Whole subtrees can be operated on at a time.
```haskell

tree = EMM.lookup (o &: K t) $  orchards
apple = EMM.lookup (K a) tree
newOrchards = delete (K o) $ orchards
EMM.lookup (o :& t :& K a) newOrchards == Nothing
```

_EnumMapSet_

There is also 'EnumMapSet'.  The terminating key type is S instead of K.

_Caveats_

EnumMapMap has grown quite an unwieldy, or at least verbose, API.  See the unit
tests for details.  Run the benchmarking suite to see how EnumMapMap compares to
IntMap for speed.

TODO:

- Finish operations on subtrees: alter
- EnumMapMap & EnumMapSet - intersectSet, differenceSet
- Check that Strict really is strict and Lazy really is lazy.
- More functions - mapMaybe, update, mergeWithKey,  foldr'
- More benchmarks and optimisation
- More documentation
- More tests
- Replace d1..d10 with numbers

