{-# LANGUAGE MagicHash, DeriveDataTypeable, StandaloneDeriving,
  TypeFamilies, BangPatterns, FlexibleContexts, FlexibleInstances,
  MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMapMap.Base
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
-- This defines the data structures and core (hidden) manipulations
-- on representations.  Based on Data.IntMap.Base
-----------------------------------------------------------------------------

module Data.EnumMapMap.Base (
            -- * Map type
            EnumMapMap,
            Key1 (..),
            Key2 (..),
            Key3 (..),
            Key4 (..),
            null,
            empty,
            size,
            find,
            lookup,
            member,
            singleton,
            insert,
            insertWith,
            insertWithKey,
            delete,
            foldrWithKey,
            foldrWithKey1,
            foldrWithKey2,
            foldrWithKey3,
            foldrWithKey4,
            map,
            map1,
            map2,
            map3,
            map4,
            mapWithKey,
            mapWithKey1,
            mapWithKey2,
            mapWithKey3,
            mapWithKey4,
            unions,
            union,
            union1,
            union2,
            union3,
            union4,
            unionWith,
            unionWith1,
            unionWith2,
            unionWith3,
            unionWith4,
            unionWithKey,
            unionWithKey1,
            unionWithKey2,
            unionWithKey3,
            unionWithKey4,
            difference4,
            difference3,
            difference2,
            difference1,
            differenceWith4,
            differenceWith3,
            differenceWith2,
            differenceWith1,
            differenceWithKey4,
            differenceWithKey3,
            differenceWithKey2,
            differenceWithKey1,
            intersectionWithKey1,
            intersectionWithKey2,
            intersectionWithKey3,
            intersectionWithKey4,
            intersectionWith1,
            intersectionWith2,
            intersectionWith3,
            intersectionWith4,
            intersection1,
            intersection2,
            intersection3,
            intersection4,
            mergeWithKey1,
            fromList,
            fromList1,
            fromList2,
            fromList3,
            fromList4,
            toList,
            toList1,
            toList2,
            toList3,
            toList4
) where

import           Prelude hiding (lookup,map,filter,foldr,foldl,null)

import           Control.Applicative (Applicative(pure,(<*>)),(<$>))
import           Control.DeepSeq (NFData(rnf))
import           Data.Bits
import qualified Data.Foldable as Foldable
import           Data.Monoid (Monoid(..))
import           Data.Traversable (Traversable(traverse))
import           GHC.Exts (Word(..), Int(..), shiftRL#)

data EMM k v = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask
                     !(EMM k v) !(EMM k v)
               | Tip {-# UNPACK #-} !Int v
               | Nil
                 deriving (Show)

type Nat    = Word
type Key    = Int
type Prefix = Int
type Mask   = Int

type EMM4 a b c d v = EMM a (EMM b (EMM c (EMM d v)))
type EMM3 a b c   v =        EMM a (EMM b (EMM c v))
type EMM2 a b     v =               EMM a (EMM b v)
type EMM1 a       v =                      EMM a v

data Key4 a b c d = Key4 a b c d
                    deriving (Show)
data Key3 a b c   = Key3 a b c
                    deriving (Show)
data Key2 a b     = Key2 a b
                    deriving (Show)
data Key1 a       = Key1 a
                    deriving (Show)
data Key0         = Key0
                    deriving (Show)

class KEMM k v where
    type EnumMapMap k v :: *
    type List k v :: *
    type PubKey k :: *
    type TailKey k :: *
    type Tail k v :: *
    type HeadKey k :: *
    tailKey :: k -> (TailKey k)
    headKey :: k -> Key
    -- Dive applies a function to every map in turn until it finds the map
    -- with a value, which it transforms.  This is the basis for many operations.
    -- Dive and associated functions must be defined for every instance so that
    -- the compiler can check that it doesn't recurse forever.
    dive :: ((Tail k v -> t) -> Key -> EnumMapMap k v -> t)
         -> (TailKey k -> Tail k v -> t)
         -> k -> EnumMapMap k v -> t
    walk :: ((Tail k v -> Tail k v)
                  -> Key -> EnumMapMap k v -> EnumMapMap k v)
         -> (TailKey k -> Tail k v -> Tail k v)
         -> k -> EnumMapMap k v -> EnumMapMap k v
    lookup :: k -> EnumMapMap k v -> Maybe v
    member :: k -> EnumMapMap k v -> Bool
    singleton :: k -> v -> EnumMapMap k v
    insert :: k -> v -> EnumMapMap k v -> EnumMapMap k v
    insertWith :: (v -> v -> v)
               -> k -> v
               -> EnumMapMap k v
               -> EnumMapMap k v
    insertWith f = insertWithKey (\_ -> f)
    insertWithKey :: (k -> v -> v -> v)
                  -> k -> v
                  -> EnumMapMap k v
                  -> EnumMapMap k v
    delete :: k -> EnumMapMap k v -> EnumMapMap k v
    foldrWithKey :: (k -> v -> t -> t) -> t -> EnumMapMap k v -> t
    foldrWithKey' :: (k -> v -> t -> t) -> t -> EnumMapMap k v -> t
    map :: (v -> t) -> EnumMapMap k v -> EnumMapMap k t
    mapWithKey :: (k -> v -> t) -> EnumMapMap k v -> EnumMapMap k t
    unions :: [EnumMapMap k v] -> EnumMapMap k v
    union :: EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    unionWithKey :: (k -> v -> v -> v)
                 -> EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    unionWith :: (v -> v -> v)
              -> EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    toList :: EnumMapMap k v -> List k v
    toAscList :: EnumMapMap k v -> List k v
    fromList :: List k v -> EnumMapMap k v

instance (Enum a, Enum b, Enum c, Enum d) => KEMM (Key4 a b c d) v where
    type EnumMapMap (Key4 a b c d) v = EMM4 a b c d v
    type List (Key4 a b c d) v = [(a, [(b, [(c, [(d, v)])])])]
    type PubKey (Key4 a b c d) = (a, b, c, d)
    type TailKey (Key4 a b c d) = Key3 b c d
    type Tail (Key4 a b c d) v = EMM3 b c d v
    type HeadKey (Key4 a b c d) = a
    tailKey (Key4 _ k2 k3 k4) = Key3 k2 k3 k4
    headKey (Key4 k1 _ _ _) = fromEnum k1
    dive f g !key = f (g $ tailKey key) $ headKey key
    walk f g !key = f (g $ tailKey key) $ headKey key
    lookup
        = dive lookup_ lookup
    member !key
        = member_ (member $ tailKey key) $ headKey key
    singleton key val = Tip (headKey key) (singleton (tailKey key) val)
    insert !key val
        = insertWithKey_ go (headKey key) $ singleton (tailKey key) val
              where
                go _ _ = insert (tailKey key) val
    insertWithKey f !key val
        = insertWithKey_ go (headKey key) $ singleton (tailKey key) val
              where
                go _ _ = insertWithKey (\_ -> f key) (tailKey key) val
    delete = walk alter_ delete
    foldrWithKey = foldrWithKey4
    foldrWithKey' = foldrWithKey4'
    map = map4
    mapWithKey = mapWithKey4
    unions = foldlStrict union4 empty
    union = union4
    unionWithKey = unionWithKey4
    unionWith f = unionWithKey4 (\_ -> f)
    toList = toAscList4
    toAscList = toAscList4
    fromList = fromList4

instance (Enum a, Enum b, Enum c) => KEMM (Key3 a b c) v where
    type EnumMapMap (Key3 a b c) v = EMM3 a b c v
    type List (Key3 a b c) v = [(a, [(b, [(c, v)])])]
    type PubKey (Key3 a b c) = (a, b, c)
    type TailKey (Key3 a b c) = Key2 b c
    type Tail (Key3 a b c) v = EMM2 b c v
    tailKey (Key3 _ k2 k3) = Key2 k2 k3
    type HeadKey (Key3 a b c) = a
    headKey (Key3 k1 _ _) = fromEnum k1
    dive f g !key = f (g $ tailKey key) $ headKey key
    walk f g !key = f (g $ tailKey key) $ headKey key
    lookup
        = dive lookup_ lookup
    member !key
        = member_ (member $ tailKey key) $ headKey key
    singleton key val = Tip (headKey key) $ singleton (tailKey key) val
    insert !key val
        = insertWithKey_ go (headKey key) $ singleton (tailKey key) val
              where
                go _ _ = insert (tailKey key) val
    insertWithKey f !key val
        = insertWithKey_ go (headKey key) $ singleton (tailKey key) val
              where
                go _ _ = insertWithKey (\_ -> f key) (tailKey key) val
    delete = walk alter_ delete
    foldrWithKey = foldrWithKey3
    foldrWithKey' = foldrWithKey3'
    map = map3
    mapWithKey = mapWithKey3
    unions = foldlStrict union3 empty
    union = union3
    unionWithKey = unionWithKey3
    unionWith f = unionWithKey3 (\_ -> f)
    toList = toAscList3
    toAscList = toAscList3
    fromList = fromList3

instance (Enum a, Enum b) => KEMM (Key2 a b) v where
    type EnumMapMap (Key2 a b) v = EMM2 a b v
    type List (Key2 a b) v = [(a, [(b, v)])]
    type PubKey (Key2 a b) = (a, b)
    type TailKey (Key2 a b) = Key1 b
    type Tail (Key2 a b) v = EMM1 b v
    tailKey (Key2 _ k2) = Key1 k2
    type HeadKey (Key2 a b) = a
    headKey (Key2 k1 _) = fromEnum k1
    dive f g key = f (g $ tailKey key) $ headKey key
    walk f g !key = f (g $ tailKey key) $ headKey key
    lookup
        = dive lookup_ lookup
    member !key
        = member_ (member $ tailKey key) $ headKey key
    singleton key val = Tip (headKey key) $ singleton (tailKey key) val
    insert !key val
        = insertWithKey_ go (headKey key) $ singleton (tailKey key) val
              where
                go _ _ = insert (tailKey key) val
    insertWithKey f !key val
        = insertWithKey_ go (headKey key) $ singleton (tailKey key) val
              where
                go _ _ = insertWithKey (\_ -> f key) (tailKey key) val
    delete = walk alter_ delete
    foldrWithKey = foldrWithKey2
    foldrWithKey' = foldrWithKey2'
    map = map2
    mapWithKey = mapWithKey2
    unions = foldlStrict union2 empty
    union = union2
    unionWithKey = unionWithKey2
    unionWith f = unionWithKey2 (\_ -> f)
    toList = toAscList2
    toAscList = toAscList2
    fromList = fromList2

instance (Enum a) => KEMM (Key1 a) v where
    type EnumMapMap (Key1 a) v = EMM1 a v
    type List (Key1 a) v = [(a, v)]
    type PubKey (Key1 a) = a
    type TailKey (Key1 a) = Key0
    type Tail (Key1 a) v = v
    tailKey (Key1 _) = Key0
    type HeadKey (Key1 a) = a
    headKey (Key1 k1) = fromEnum k1
    dive f g key = f (g $ tailKey key) $ headKey key
    walk f g !key = f (g $ tailKey key) $ headKey key
    lookup
        = dive lookup_ (\_ -> Just)
    member !key
        = member_ (\_ -> True) $ headKey key
    singleton key val = Tip (headKey key) val
    insert !k
        = go (headKey k)
          where
            go key val emm =
                case emm of
                  Bin p m l r
                      | nomatch key p m -> join key (Tip key val) p emm
                      | zero key m      -> Bin p m (go key val l) r
                      | otherwise       -> Bin p m l (go key val r)
                  Tip ky _
                      | key == ky       -> Tip key val
                      | otherwise       -> join key (Tip key val) ky emm
                  Nil                   -> Tip key val
    insertWithKey f !key val emm
        = insertWithKey_ (\_ -> f key) (headKey key) val emm
    delete = go . headKey
        where go !k t =
                  case t of
                    Bin p m l r | nomatch k p m -> t
                                | zero k m      -> bin p m (go k l) r
                                | otherwise     -> bin p m l (go k r)
                    Tip ky _ | k == ky          -> Nil
                             | otherwise        -> t
                    Nil                         -> Nil
    foldrWithKey = foldrWithKey1
    foldrWithKey' = foldrWithKey1'
    map = map1
    mapWithKey = mapWithKey1
    unions = foldlStrict union1 empty
    union = union1
    unionWithKey = unionWithKey1
    unionWith f = unionWithKey1 (\_ -> f)
    toList = toAscList1
    toAscList = toAscList1
    fromList = fromList1

{---------------------------------------------------------------------
 Instances
---------------------------------------------------------------------}

-- Eq

instance Eq v => Eq (EMM k v) where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: Eq v => EMM k v -> EMM k v -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False

nequal :: Eq v => EMM k v -> EMM k v -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequal Nil Nil = False
nequal _   _   = True

instance (Enum a) => Functor (EMM a)
    where
      fmap f = mapWithKey_ (\_ -> f)

instance (Enum k) => Monoid (EMM k v) where
    mempty = empty
    mappend = union1
    mconcat = foldlStrict union1 empty

instance Foldable.Foldable (EMM a) where
  fold Nil = mempty
  fold (Tip _ v) = v
  fold (Bin _ _ l r) = Foldable.fold l `mappend` Foldable.fold r
  foldMap _ Nil = mempty
  foldMap f (Tip _k v) = f v
  foldMap f (Bin _ _ l r) = Foldable.foldMap f l `mappend` Foldable.foldMap f r

instance Enum a => Traversable (EMM a) where
    traverse _ Nil = pure Nil
    traverse f (Tip k v) = Tip k <$> f v
    traverse f (Bin p m l r) = Bin p m <$> traverse f l <*> traverse f r

instance NFData v => NFData (EMM a v) where
    rnf Nil = ()
    rnf (Tip _ v) = rnf v
    rnf (Bin _ _ l r) = rnf l `seq` rnf r

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

null :: EMM a v -> Bool
null t = case t of
           Nil -> True
           _   -> False

size :: EMM a v -> Int
size t = case t of
           Bin _ _ l r -> size l + size r
           Tip _ _     -> 1
           Nil         -> 0

empty :: EMM a v
empty = Nil

find :: (KEMM k v, Show k) => k -> EnumMapMap k v -> v
find key imm =
    case lookup key imm of
      Nothing -> error ("EnumMapMap.find: key " ++ show key ++
                        " is not an element of the map")
      Just x  -> x

lookup_ :: (v -> Maybe t) -> Key -> EMM a v -> Maybe t
lookup_ f !key (Bin _ m l r)
          | zero key m  = lookup_ f key l
          | otherwise = lookup_ f key r
lookup_ f !key (Tip kx x)
          | key == kx = f x
          | otherwise = Nothing
lookup_ _ _ Nil = Nothing

member_ :: (v -> Bool ) -> Key -> EMM a v -> Bool
member_ f !key t =
    case t of
      Bin _ m l r -> case zero key m of
                      True  -> member_ f key l
                      False -> member_ f key r
      Tip kx x -> case key == kx of
                    True -> f x
                    False -> False
      Nil -> False

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}

-- | Insert with a combining function.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f key new_value old_value@.
--
insertWithKey_ :: Enum a =>
                  (Key -> v -> v -> v) -> Key -> v -> EMM a v -> EMM a v
insertWithKey_ f !k x t =
    case t of
      Bin p m l r
          | nomatch k p m -> join k (Tip k x) p t
          | zero k m      -> Bin p m (insertWithKey_ f k x l) r
          | otherwise     -> Bin p m l (insertWithKey_ f k x r)
      Tip ky y
          | k == ky       -> Tip k (f k x y)
          | otherwise     -> join k (Tip k x) ky t
      Nil                 -> Tip k x

{---------------------------------------------------------------------
  Delete/Update/Adjust/Alter
---------------------------------------------------------------------}

-- 'alter_' is used to walk down the tree to find the EMM to actually change.  If
-- the new EMM is Nil then it's removed from the containing EMM.
alter_ :: (EMM b v -> EMM b v) -> Key -> EMM a (EMM b v) -> EMM a (EMM b v)
alter_ f = go
    where go !k t =
              case t of
                Bin p m l r | nomatch k p m -> t
                            | zero k m      -> binD p m (go k l) r
                            | otherwise     -> binD p m l (go k r)
                Tip ky y | k == ky          -> tip k $ f y
                         | otherwise        -> t
                Nil                         -> Nil

{---------------------------------------------------------------------
  Map
---------------------------------------------------------------------}

map4 :: (Enum a, Enum b, Enum c, Enum d) =>
        (v -> t)
     -> EnumMapMap (Key4 a b c d) v
     -> EnumMapMap (Key4 a b c d) t
map4 f = mapWithKey4 (\_ -> f)

map3 :: (Enum a, Enum b, Enum c) =>
        (v -> t)
     -> EnumMapMap (Key3 a b c) v
     -> EnumMapMap (Key3 a b c) t
map3 f = mapWithKey3 (\_ -> f)

map2 :: (Enum a, Enum b) =>
        (v -> t)
     -> EnumMapMap (Key2 a b) v
     -> EnumMapMap (Key2 a b) t
map2 f = mapWithKey2 (\_ -> f)

map1 ::  Enum a =>
         (v -> t)
     -> EnumMapMap (Key1 a) v
     -> EnumMapMap (Key1 a) t
map1 f = mapWithKey1 (\_ -> f)

mapWithKey4 :: (Enum a, Enum b, Enum c, Enum d) =>
               (Key4 a b c d -> v -> t)
            -> EnumMapMap (Key4 a b c d) v
            -> EnumMapMap (Key4 a b c d) t
mapWithKey4 f = mapWithKey_ go
    where go k1 = mapWithKey3 (\(Key3 k2 k3 k4) -> f $ Key4 k1 k2 k3 k4)

mapWithKey3 :: (Enum a, Enum b, Enum c) =>
               (Key3 a b c -> v -> t)
            -> EnumMapMap (Key3 a b c) v
            -> EnumMapMap (Key3 a b c) t
mapWithKey3 f = mapWithKey_ go
    where go k1 = mapWithKey2 (\(Key2 k2 k3) -> f $ Key3 k1 k2 k3)

mapWithKey2 :: (Enum a, Enum b) =>
               (Key2 a b -> v -> t)
            -> EnumMapMap (Key2 a b) v
            -> EnumMapMap (Key2 a b) t
mapWithKey2 f = mapWithKey_ go
    where go k1 = mapWithKey1 (\(Key1 k2) -> f $ Key2 k1 k2)

mapWithKey1 :: Enum a =>
               (Key1 a -> v -> t)
            -> EnumMapMap (Key1 a) v
            -> EnumMapMap (Key1 a) t
mapWithKey1 f = mapWithKey_ (f . Key1)

mapWithKey_ :: Enum k => (k -> v -> t) -> EMM k v -> EMM k t
mapWithKey_ f = go
    where
      go (Bin p m l r) = Bin p m (go l) (go r)
      go (Tip k x)     = Tip k (f (toEnum k) x)
      go Nil           = Nil

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

foldrWithKey1 :: (Enum a) =>
                 ((Key1 a) -> v -> t -> t)
              -> t -> EnumMapMap (Key1 a) v -> t
foldrWithKey1 f = foldrWithKey_ (f . Key1)
{-# INLINE foldrWithKey1 #-}

foldrWithKey2 :: (Enum a, Enum b) =>
                 ((Key2 a b) -> v -> t -> t)
              -> t -> EnumMapMap (Key2 a b) v -> t
foldrWithKey2 f = foldrWithKey_ go
    where
      go k1 z t = foldrWithKey1 (\(Key1 k2) -> f $ Key2 k1 k2) t z
{-# INLINE foldrWithKey2 #-}

foldrWithKey3 :: (Enum a, Enum b, Enum c) =>
                 ((Key3 a b c) -> v -> t -> t)
              -> t -> EnumMapMap (Key3 a b c) v -> t
foldrWithKey3 f = foldrWithKey_ go
    where
      go k1 z t = foldrWithKey2 (\(Key2 k2 k3) -> f $ Key3 k1 k2 k3) t z
{-# INLINE foldrWithKey3 #-}

foldrWithKey4 :: (Enum a, Enum b, Enum c, Enum d) =>
                 ((Key4 a b c d) -> v -> t -> t)
              -> t -> EnumMapMap (Key4 a b c d) v -> t
foldrWithKey4 f = foldrWithKey_ go
    where
      go k1 z t = foldrWithKey3 (\(Key3 k2 k3 k4) -> f $ Key4 k1 k2 k3 k4) t z
{-# INLINE foldrWithKey4 #-}

foldrWithKey_ :: (Enum k) => (k -> b -> t -> t) -> t -> EMM k b -> t
foldrWithKey_ f z = \t ->
    case t of Bin _ m l r | m < 0     -> go (go z l) r
                          | otherwise -> go (go z r) l
              _                       -> go z t
    where
      go z' Nil           = z'
      go z' (Tip kx tx)   = f (toEnum kx) tx z'
      go z' (Bin _ _ l r) = go (go z' r) l

-- Strict foldrWithKey_
foldrWithKey1' :: (Enum a) =>
                 ((Key1 a) -> v -> t -> t)
              -> t -> EnumMapMap (Key1 a) v -> t
foldrWithKey1' f = foldrWithKey_' (\k1 -> f $ Key1 k1)
{-# INLINE foldrWithKey1' #-}

foldrWithKey2' :: (Enum a, Enum b) =>
                 ((Key2 a b) -> v -> t -> t)
              -> t -> EnumMapMap (Key2 a b) v -> t
foldrWithKey2' f = foldrWithKey_' go
    where
      go k1 z t = foldrWithKey1' (\(Key1 k2) -> f $ Key2 k1 k2) t z
{-# INLINE foldrWithKey2' #-}

foldrWithKey3' :: (Enum a, Enum b, Enum c) =>
                 ((Key3 a b c) -> v -> t -> t)
              -> t -> EnumMapMap (Key3 a b c) v -> t
foldrWithKey3' f = foldrWithKey_' go
    where
      go k1 z t = foldrWithKey2' (\(Key2 k2 k3) -> f $ Key3 k1 k2 k3) t z
{-# INLINE foldrWithKey3' #-}

foldrWithKey4' :: (Enum a, Enum b, Enum c, Enum d) =>
                 ((Key4 a b c d) -> v -> t -> t)
              -> t -> EnumMapMap (Key4 a b c d) v -> t
foldrWithKey4' f = foldrWithKey_' go
    where
      go k1 z t = foldrWithKey3' (\(Key3 k2 k3 k4) -> f $ Key4 k1 k2 k3 k4) t z
{-# INLINE foldrWithKey4' #-}

foldrWithKey_' :: (Enum k) => (k -> b -> t -> t) -> t -> EMM k b -> t
foldrWithKey_' f z = \t ->
    case t of Bin _ m l r | m < 0     -> go (go z l) r
                          | otherwise -> go (go z r) l
              _                       -> go z t
    where
      go !z' Nil           = z'
      go !z' (Tip kx tx)   = f (toEnum kx) tx z'
      go !z' (Bin _ _ l r) = go (go z' r) l

{-# INLINE foldrWithKey_' #-}

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

toAscList4 :: (Enum a, Enum b, Enum c, Enum d) =>
              EnumMapMap (Key4 a b c d) v -> List (Key4 a b c d) v
toAscList4 = toAscList_ toAscList3

toAscList3 :: (Enum a, Enum b, Enum c) =>
              EnumMapMap (Key3 a b c) v -> List (Key3 a b c) v
toAscList3 = toAscList_ toAscList2

toAscList2 :: (Enum a, Enum b) =>
              EnumMapMap (Key2 a b) v -> List (Key2 a b) v
toAscList2 = toAscList_ toAscList1

toAscList1 :: Enum a => EnumMapMap (Key1 a) v -> List (Key1 a) v
toAscList1 = toAscList_ id

toAscList_ :: Enum k => (v -> t) -> EMM k v -> [(k, t)]
toAscList_ f = foldrWithKey_ (\key x xs -> (key, f x):xs) []
{-# INLINE toAscList_ #-}

toList4 :: (Enum a, Enum b, Enum c, Enum d) =>
           EnumMapMap (Key4 a b c d) v -> List (Key4 a b c d) v
toList4 = toAscList4

toList3 :: (Enum a, Enum b, Enum c) =>
           EnumMapMap (Key3 a b c) v -> List (Key3 a b c) v
toList3 = toAscList3

toList2 :: (Enum a, Enum b) =>
           EnumMapMap (Key2 a b) v -> List (Key2 a b) v
toList2 = toAscList2

toList1 ::  Enum a => EnumMapMap (Key1 a) v -> List (Key1 a) v
toList1 = toAscList1

fromList4 :: (Enum a, Enum b, Enum c, Enum d) =>
             List (Key4 a b c d) v -> EnumMapMap (Key4 a b c d) v
fromList4 = fromList_ fromList3

fromList3 :: (Enum a, Enum b, Enum c) =>
             List (Key3 a b c) v -> EnumMapMap (Key3 a b c) v
fromList3 = fromList_ fromList2

fromList2 :: (Enum a, Enum b) =>
             List (Key2 a b) v -> EnumMapMap (Key2 a b) v
fromList2 = fromList_ fromList1

fromList1 :: Enum a => List (Key1 a) v -> EnumMapMap (Key1 a) v
fromList1 = fromList_ id

fromList_ :: Enum k => (t -> v) -> [(k, t)] -> EMM k v
fromList_ f = foldlStrict ins empty
    where
      ins t (k1, x) = insert (Key1 k1) (f x) t
{-# INLINE fromList_ #-}

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}

union4 :: (Enum a, Enum b, Enum c, Enum d) =>
          EnumMapMap (Key4 a b c d) v
       -> EnumMapMap (Key4 a b c d) v
       -> EnumMapMap (Key4 a b c d) v
union4 = mergeWithKey' binD go id id
    where
      go = \(Tip k1 x1) (Tip _ x2) ->
           tip k1 $ union3 x1 x2
      {-# INLINE go #-}

union3 :: (Enum a, Enum b, Enum c) =>
          EnumMapMap (Key3 a b c) v
       -> EnumMapMap (Key3 a b c) v
       -> EnumMapMap (Key3 a b c) v
union3 = mergeWithKey' binD go id id
    where
      go = \(Tip k1 x1) (Tip _ x2) ->
           tip k1 $ union2 x1 x2
      {-# INLINE go #-}

union2 :: (Enum a, Enum b) =>
          EnumMapMap (Key2 a b) v
       -> EnumMapMap (Key2 a b) v
       -> EnumMapMap (Key2 a b) v
union2 = mergeWithKey' binD go id id
    where
      go = \(Tip k1 x1) (Tip _ x2) ->
           tip k1 $ union1 x1 x2
      {-# INLINE go #-}

union1 :: Enum a =>
          EnumMapMap (Key1 a) v
       -> EnumMapMap (Key1 a) v
       -> EnumMapMap (Key1 a) v
union1 = mergeWithKey' Bin const id id

unionWith4 :: (Enum a, Enum b, Enum c, Enum d) =>
              (v -> v -> v)
           -> EnumMapMap (Key4 a b c d) v
           -> EnumMapMap (Key4 a b c d) v
           -> EnumMapMap (Key4 a b c d) v
unionWith4 f = unionWithKey4 (\_ -> f)

unionWith3 :: (Enum a, Enum b, Enum c) =>
                 (v -> v -> v)
              -> EnumMapMap (Key3 a b c) v
              -> EnumMapMap (Key3 a b c) v
              -> EnumMapMap (Key3 a b c) v
unionWith3 f = unionWithKey3 (\_ -> f)

unionWith2 :: (Enum a, Enum b) =>
              (v -> v -> v)
           -> EnumMapMap (Key2 a b) v
           -> EnumMapMap (Key2 a b) v
           -> EnumMapMap (Key2 a b) v
unionWith2 f = unionWithKey2 (\_ -> f)

unionWith1 :: (Enum a) =>
              (v -> v -> v)
           -> EnumMapMap (Key1 a) v
           -> EnumMapMap (Key1 a) v
           -> EnumMapMap (Key1 a) v
unionWith1 f = unionWithKey1 (\_ -> f)

unionWithKey4 :: (Enum a, Enum b, Enum c, Enum d) =>
                 (Key4 a b c d -> v -> v -> v)
              -> EnumMapMap (Key4 a b c d) v
              -> EnumMapMap (Key4 a b c d) v
              -> EnumMapMap (Key4 a b c d) v
unionWithKey4 f = mergeWithKey' binD go id id
    where
      go = \(Tip k1 x1) (Tip _ x2) ->
           tip k1 $ unionWithKey3 (\(Key3 k2 k3 k4) ->
                                       f $ Key4 (toEnum k1) k2 k3 k4) x1 x2
      {-# INLINE go #-}

unionWithKey3 :: (Enum a, Enum b, Enum c) =>
                 (Key3 a b c -> v -> v -> v)
              -> EnumMapMap (Key3 a b c) v
              -> EnumMapMap (Key3 a b c) v
              -> EnumMapMap (Key3 a b c) v
unionWithKey3 f = mergeWithKey' binD go id id
    where
      go = \(Tip k1 x1) (Tip _ x2) ->
           tip k1 $ unionWithKey2 (\(Key2 k2 k3) ->
                                       f $ Key3 (toEnum k1) k2 k3) x1 x2
      {-# INLINE go #-}

unionWithKey2 :: (Enum a, Enum b) =>
                 (Key2 a b -> v -> v -> v)
              -> EnumMapMap (Key2 a b) v
              -> EnumMapMap (Key2 a b) v
              -> EnumMapMap (Key2 a b) v
unionWithKey2 f = mergeWithKey' binD go id id
    where
      go = \(Tip k1 x1) (Tip _ x2) ->
           tip k1 $ unionWithKey1 (\(Key1 k2) -> f $ Key2 (toEnum k1) k2) x1 x2
      {-# INLINE go #-}

unionWithKey1 :: (Enum a) =>
                 (Key1 a -> v -> v -> v)
              -> EnumMapMap (Key1 a) v
              -> EnumMapMap (Key1 a) v
              -> EnumMapMap (Key1 a) v
unionWithKey1 f = mergeWithKey' Bin go id id
    where
      go = \(Tip k1 x1) (Tip _ x2) ->
           Tip k1 $ f (Key1 $ toEnum k1) x1 x2
      {-# INLINE go #-}

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

differenceWithKey4 :: (Enum a, Enum b, Enum c, Enum d) =>
                      (Key4 a b c d -> v1 -> v2 -> Maybe v1)
                   -> EnumMapMap (Key4 a b c d) v1
                   -> EnumMapMap (Key4 a b c d) v2
                   -> EnumMapMap (Key4 a b c d) v1
differenceWithKey4 f = differenceWithKey_ go
    where go k1 = differenceWithKey3 (\(Key3 k2 k3 k4)
                                          -> f $ Key4 (toEnum k1) k2 k3 k4)

differenceWithKey3 :: (Enum a, Enum b, Enum c) =>
                      (Key3 a b c -> v1 -> v2 -> Maybe v1)
                   -> EnumMapMap (Key3 a b c) v1
                   -> EnumMapMap (Key3 a b c) v2
                   -> EnumMapMap (Key3 a b c) v1
differenceWithKey3 f = differenceWithKey_ go
    where go k1 = differenceWithKey2 (\(Key2 k2 k3) -> f $ Key3 (toEnum k1) k2 k3)

differenceWithKey2 :: (Enum a, Enum b) =>
                      (Key2 a b -> v1 -> v2 -> Maybe v1)
                   -> EnumMapMap (Key2 a b) v1
                   -> EnumMapMap (Key2 a b) v2
                   -> EnumMapMap (Key2 a b) v1
differenceWithKey2 f = differenceWithKey_ go
    where go k1 = differenceWithKey1 (\(Key1 k2) -> f $ Key2 (toEnum k1) k2)

differenceWithKey1 :: (Enum a) =>
                      (Key1 a -> v1 -> v2 -> Maybe v1)
                   -> EnumMapMap (Key1 a) v1
                   -> EnumMapMap (Key1 a) v2
                   -> EnumMapMap (Key1 a) v1
differenceWithKey1 f = mergeWithKey' bin combine id (const Nil)
  where
    combine = \(Tip k1 x1) (Tip _ x2)
            -> case f (Key1 $ toEnum k1) x1 x2 of Nothing -> Nil
                                                  Just x -> Tip k1 x
    {-# INLINE combine #-}

differenceWithKey_ :: (Enum a, Enum b) =>
                      (Key -> EMM b v1 -> EMM b v2 -> EMM b v1)
                   -> EMM a (EMM b v1) -> EMM a (EMM b v2) -> EMM a (EMM b v1)
differenceWithKey_ f =
    mergeWithKey' binD (\(Tip k1 x1) (Tip _k2 x2) ->
                              tip k1 (f k1 x1 x2)) id (const Nil)

differenceWith4 :: (Enum a, Enum b, Enum c, Enum d) =>
                   (v1 -> v2 -> Maybe v1)
                -> EnumMapMap (Key4 a b c d) v1
                -> EnumMapMap (Key4 a b c d) v2
                -> EnumMapMap (Key4 a b c d) v1
differenceWith4 f
    = differenceWithKey4 (\_ -> f)

differenceWith3 :: (Enum a, Enum b, Enum c) =>
                   (v1 -> v2 -> Maybe v1)
                -> EnumMapMap (Key3 a b c) v1
                -> EnumMapMap (Key3 a b c) v2
                -> EnumMapMap (Key3 a b c) v1
differenceWith3 f
    = differenceWithKey3 (\_ -> f)

differenceWith2 :: (Enum a, Enum b) =>
                   (v1 -> v2 -> Maybe v1)
                -> EnumMapMap (Key2 a b) v1
                -> EnumMapMap (Key2 a b) v2
                -> EnumMapMap (Key2 a b) v1
differenceWith2 f
    = differenceWithKey2 (\_ -> f)

differenceWith1 :: (Enum a) =>
                   (v1 -> v2 -> Maybe v1)
                -> EnumMapMap (Key1 a) v1
                -> EnumMapMap (Key1 a) v2
                -> EnumMapMap (Key1 a) v1
differenceWith1 f
    = differenceWithKey1 (\_ -> f)

difference4 :: (Enum a, Enum b, Enum c, Enum d) =>
               EnumMapMap (Key4 a b c d) v1
            -> EnumMapMap (Key4 a b c d) v2
            -> EnumMapMap (Key4 a b c d) v1
difference4
    = mergeWithKey' bin (\_ _ -> Nil) id (const Nil)

difference3 :: (Enum a, Enum b, Enum c) =>
               EnumMapMap (Key3 a b c) v1
            -> EnumMapMap (Key3 a b c) v2
            -> EnumMapMap (Key3 a b c) v1
difference3
    = mergeWithKey' bin (\_ _ -> Nil) id (const Nil)

difference2 :: (Enum a, Enum b) =>
               EnumMapMap (Key2 a b) v1
            -> EnumMapMap (Key2 a b) v2
            -> EnumMapMap (Key2 a b) v1
difference2
    = mergeWithKey' bin (\_ _ -> Nil) id (const Nil)

difference1 :: (Enum a) =>
               EnumMapMap (Key1 a) v1
            -> EnumMapMap (Key1 a) v2
            -> EnumMapMap (Key1 a) v1
difference1
    = mergeWithKey' bin (\_ _ -> Nil) id (const Nil)

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

intersectionWithKey4 :: (Enum a, Enum b, Enum c, Enum d) =>
                        (Key4 a b c d -> v1 -> v2 -> v3)
                     -> EnumMapMap (Key4 a b c d) v1
                     -> EnumMapMap (Key4 a b c d) v2
                     -> EnumMapMap (Key4 a b c d) v3
intersectionWithKey4 f = intersectionWithKey_ go
    where
      go k1 = intersectionWithKey3 (\(Key3 k2 k3 k4) -> f $ Key4 (toEnum k1) k2
                                                        k3 k4)

intersectionWithKey3 :: (Enum a, Enum b, Enum c) =>
                        (Key3 a b c -> v1 -> v2 -> v3)
                     -> EnumMapMap (Key3 a b c) v1
                     -> EnumMapMap (Key3 a b c) v2
                     -> EnumMapMap (Key3 a b c) v3
intersectionWithKey3 f = intersectionWithKey_ go
    where
      go k1 = intersectionWithKey2 (\(Key2 k2 k3) -> f $ Key3 (toEnum k1) k2 k3)

intersectionWithKey2 :: (Enum a, Enum b) =>
                        (Key2 a b -> v1 -> v2 -> v3)
                     -> EnumMapMap (Key2 a b) v1
                     -> EnumMapMap (Key2 a b) v2
                     -> EnumMapMap (Key2 a b) v3
intersectionWithKey2 f = intersectionWithKey_ go
    where go k1 = intersectionWithKey1 (\(Key1 k2) -> f $ Key2 (toEnum k1) k2)

intersectionWithKey1 :: (Enum a) =>
                        (Key1 a -> v1 -> v2 -> v3)
                     -> EnumMapMap (Key1 a) v1
                     -> EnumMapMap (Key1 a) v2
                     -> EnumMapMap (Key1 a) v3
intersectionWithKey1 f
    = mergeWithKey' bin go (const Nil) (const Nil)
          where
            go = \(Tip k1 x1) (Tip _k2 x2) ->
                 Tip k1 (f (Key1 $ toEnum k1) x1 x2)

intersectionWithKey_ :: (Enum a, Enum b) =>
                        (Key -> EMM b v1 -> EMM b v2 -> EMM b v3)
                     -> EMM a (EMM b v1) -> EMM a (EMM b v2) -> EMM a (EMM b v3)
intersectionWithKey_ f
    = mergeWithKey' binD (\(Tip k1 x1) (Tip _k2 x2) ->
                              tip k1 (f k1 x1 x2)) (const Nil) (const Nil)

intersectionWith1 :: (Enum a) =>
                     (v1 -> v2 -> v3)
                  -> EnumMapMap (Key1 a) v1
                  -> EnumMapMap (Key1 a) v2
                  -> EnumMapMap (Key1 a) v3
intersectionWith1 f = intersectionWithKey1 (\_ -> f)

intersectionWith2 :: (Enum a, Enum b) =>
                     (v1 -> v2 -> v3)
                  -> EnumMapMap (Key2 a b) v1
                  -> EnumMapMap (Key2 a b) v2
                  -> EnumMapMap (Key2 a b) v3
intersectionWith2 f = intersectionWithKey2 (\_ -> f)

intersectionWith3 :: (Enum a, Enum b, Enum c) =>
                     (v1 -> v2 -> v3)
                  -> EnumMapMap (Key3 a b c) v1
                  -> EnumMapMap (Key3 a b c) v2
                  -> EnumMapMap (Key3 a b c) v3
intersectionWith3 f = intersectionWithKey3 (\_ -> f)

intersectionWith4 :: (Enum a, Enum b, Enum c, Enum d) =>
                     (v1 -> v2 -> v3)
                  -> EnumMapMap (Key4 a b c d) v1
                  -> EnumMapMap (Key4 a b c d) v2
                  -> EnumMapMap (Key4 a b c d) v3
intersectionWith4 f = intersectionWithKey4 (\_ -> f)

intersection4 :: (Enum a, Enum b, Enum c, Enum d) =>
                 EnumMapMap (Key4 a b c d) v
              -> EnumMapMap (Key4 a b c d) v
              -> EnumMapMap (Key4 a b c d) v
intersection4 = intersectionWithKey_ (\_ -> intersection3)

intersection3 :: (Enum a, Enum b, Enum c) =>
                 EnumMapMap (Key3 a b c) v
              -> EnumMapMap (Key3 a b c) v
              -> EnumMapMap (Key3 a b c) v
intersection3 = intersectionWithKey_ (\_ -> intersection2)

intersection2 :: (Enum a, Enum b) =>
                 EnumMapMap (Key2 a b) v
              -> EnumMapMap (Key2 a b) v
              -> EnumMapMap (Key2 a b) v
intersection2 = intersectionWithKey_ (\_ -> intersection1)

intersection1 :: (Enum a) =>
                 EnumMapMap (Key1 a) v
              -> EnumMapMap (Key1 a) v
              -> EnumMapMap (Key1 a) v
intersection1 = mergeWithKey' bin const (const Nil) (const Nil)

{--------------------------------------------------------------------
  mergeWithKey
--------------------------------------------------------------------}

mergeWithKey1 :: Enum a =>
                 (Key1 a -> v1 -> v2 -> Maybe v3)
              -> (EnumMapMap (Key1 a) v1 -> EnumMapMap (Key1 a) v3)
              -> (EnumMapMap (Key1 a) v2 -> EnumMapMap (Key1 a) v3)
              -> EnumMapMap (Key1 a) v1
              -> EnumMapMap (Key1 a) v2
              -> EnumMapMap (Key1 a) v3
mergeWithKey1 f g1 g2 = mergeWithKey' bin combine g1 g2
  where
    combine = \(Tip k1 x1) (Tip _k2 x2)
            -> case f (Key1 $ toEnum k1) x1 x2 of Nothing -> Nil
                                                  Just x -> Tip k1 x
    {-# INLINE combine #-}
{-# INLINE mergeWithKey1 #-}

mergeWithKey' :: (Enum a) =>
                 (Prefix -> Mask -> EMM a v3 -> EMM a v3 -> EMM a v3)
              -> (EMM a v1 -> EMM a v2 -> EMM a v3)
              -> (EMM a v1 -> EMM a v3)
              -> (EMM a v2 -> EMM a v3)
              -> EMM a v1 -> EMM a v2 -> EMM a v3
mergeWithKey' bin' f g1 g2 = go
  where
    go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
      | shorter m1 m2  = merge1
      | shorter m2 m1  = merge2
      | p1 == p2       = bin' p1 m1 (go l1 l2) (go r1 r2)
      | otherwise      = maybe_join p1 (g1 t1) p2 (g2 t2)
      where
        merge1 | nomatch p2 p1 m1  = maybe_join p1 (g1 t1) p2 (g2 t2)
               | zero p2 m1        = bin' p1 m1 (go l1 t2) (g1 r1)
               | otherwise         = bin' p1 m1 (g1 l1) (go r1 t2)
        merge2 | nomatch p1 p2 m2  = maybe_join p1 (g1 t1) p2 (g2 t2)
               | zero p1 m2        = bin' p2 m2 (go t1 l2) (g2 r2)
               | otherwise         = bin' p2 m2 (g2 l2) (go t1 r2)

    go t1'@(Bin _ _ _ _) t2'@(Tip k2' _) = merge t2' k2' t1'
      where merge t2 k2 t1@(Bin p1 m1 l1 r1)
                | nomatch k2 p1 m1 = maybe_join p1 (g1 t1) k2 (g2 t2)
                | zero k2 m1 = bin' p1 m1 (merge t2 k2 l1) (g1 r1)
                | otherwise  = bin' p1 m1 (g1 l1) (merge t2 k2 r1)
            merge t2 k2 t1@(Tip k1 _)
                | k1 == k2 = f t1 t2
                | otherwise = maybe_join k1 (g1 t1) k2 (g2 t2)
            merge t2 _  Nil = g2 t2

    go t1@(Bin _ _ _ _) Nil = g1 t1

    go t1'@(Tip k1' _) t2' = merge t1' k1' t2'
      where merge t1 k1 t2@(Bin p2 m2 l2 r2)
                | nomatch k1 p2 m2 = maybe_join k1 (g1 t1) p2 (g2 t2)
                | zero k1 m2 = bin' p2 m2 (merge t1 k1 l2) (g2 r2)
                | otherwise  = bin' p2 m2 (g2 l2) (merge t1 k1 r2)
            merge t1 k1 t2@(Tip k2 _)
                | k1 == k2 = f t1 t2
                | otherwise = maybe_join k1 (g1 t1) k2 (g2 t2)
            merge t1 _  Nil = g1 t1

    go Nil t2 = g2 t2

    maybe_join _ Nil _ t2 = t2
    maybe_join _ t1 _ Nil = t1
    maybe_join p1 t1 p2 t2 = join p1 t1 p2 t2
    {-# INLINE maybe_join #-}
{-# INLINE mergeWithKey' #-}

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Nat conversion
--------------------------------------------------------------------}

natFromInt :: Int -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Int
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

shiftRL :: Nat -> Int -> Nat
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)

{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Prefix -> EMM a v -> Prefix -> EMM a v -> EMM a v
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE join #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> EMM k v -> EMM k v -> EMM k v
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

{--------------------------------------------------------------------
  @binD@ assures that we never have empty trees in the next level
--------------------------------------------------------------------}
binD :: Prefix -> Mask -> EMM a (EMM b v) -> EMM a (EMM b v) -> EMM a (EMM b v)
binD _ _ l Nil = l
binD _ _ Nil r = r
binD _ _ l (Tip _ Nil) = l
binD _ _ (Tip _ Nil) r = r
binD p m l r = Bin p m l r
{-# INLINE binD #-}

tip :: Key -> EMM b v -> EMM a (EMM b v)
tip _ Nil = Nil
tip !k val = Tip k val
{-# INLINE tip #-}

{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
{-# INLINE zero #-}

nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p
{-# INLINE nomatch #-}

match i p m
  = (mask i m) == p
{-# INLINE match #-}

mask :: Key -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)
{-# INLINE mask #-}

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))
{-# INLINE maskW #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)
{-# INLINE shorter #-}

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
{-# INLINE branchMask #-}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the
  highest bit and than taking an exclusive or with the original value.
  Allthough the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))
{-# INLINE highestBitMask #-}

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}

