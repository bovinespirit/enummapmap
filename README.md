EnumMapMap
==========

Based on `containers 5.0`, EnumMapMap provides a series of containers: an
EnumMap, which is an `IntMap` that uses `Enum` types instead of `Int`s, an `EnumMap` of
`EnumMap`s, an `EnumMap` of `EnumMap`s of `EnumMap`s and an `EnumMap` of `EnumMap`s of
`EnumMap`s of `EnumMap`s.

The code ensures that only the root `EnumMap` can be empty.  These containers
therefore behave differently to an `IntMap (IntMap a)`.  It is anticipated that
the enumerated index types will actually be `newtype`d `Int`s, so a 4 level
`EnumMapMap` may look like:

```haskell
newtype Id1 = Int
newtype Id2 = Int
newtype Id3 = Int
newtype Id4 = Int
type DemoMap = EnumMapMap (Key4 Id1 Id2 Id3 Id4) ValueType
```
