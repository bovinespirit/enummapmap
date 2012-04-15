{-# LANGUAGE CPP #-}
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
            EnumMapMap, Key1, Key2, Key3, Key4,
            null,
            empty,
            size,
            find,
            lookup,
            member,
            insert,
            foldrWithKey

) where

import Prelude hiding (lookup,map,filter,foldr,foldl,null)

import           Data.Bits
import qualified Data.Foldable as Foldable
import           GHC.Exts ( Word(..), Int(..), shiftRL#, build )

data EMM k v = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask
                     !(EMM k v) !(EMM k v)
               | Tip {-# UNPACK #-} !Int v
               | Nil

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
    type TailKey k :: *
    type HeadKey k :: *
    tailKey :: k -> (TailKey k)
    headKey :: k -> Key
    lookup :: k -> EnumMapMap k v -> Maybe v
    member :: k -> EnumMapMap k v -> Bool
    insert :: k -> v -> EnumMapMap k v -> EnumMapMap k v
    foldrWithKey :: (k -> v -> t -> t) -> t -> EnumMapMap k v -> t
    foldrWithKey' :: (k -> v -> t -> t) -> t -> EnumMapMap k v -> t
    toList :: EnumMapMap k v -> List k v
    toAscList :: EnumMapMap k v -> List k v
    fromList :: List k v -> EnumMapMap k v

instance (Enum a, Enum b, Enum c, Enum d) => KEMM (Key4 a b c d) v where
    type EnumMapMap (Key4 a b c d) v = EMM4 a b c d v
    type List (Key4 a b c d) v = [(a, [(b, [(c, [(d, v)])])])]
    type TailKey (Key4 a b c d) = Key3 b c d
    type HeadKey (Key4 a b c d) = a
    tailKey (Key4 _ k2 k3 k4) = Key3 k2 k3 k4
    headKey (Key4 k1 _ _ _) = fromEnum k1
    lookup !key
        = lookup_ (lookup $ tailKey key) $ headKey key
    member !key
        = member_ (member $ tailKey key) $ headKey key
    insert !key val
        = insert_ (insert (tailKey key) val) empty $ headKey key
    foldrWithKey = foldrWithKey4
    foldrWithKey' = foldrWithKey4'
    toList = toAscList4
    toAscList = toAscList4
    fromList = fromList4

instance (Enum a, Enum b, Enum c) => KEMM (Key3 a b c) v where
    type EnumMapMap (Key3 a b c) v = EMM3 a b c v
    type List (Key3 a b c) v = [(a, [(b, [(c, v)])])]
    type TailKey (Key3 a b c) = Key2 b c
    tailKey (Key3 _ k2 k3) = Key2 k2 k3
    type HeadKey (Key3 a b c) = a
    headKey (Key3 k1 _ _) = fromEnum k1
    lookup !key
        = lookup_ (lookup $ tailKey key) $ headKey key
    member !key
        = member_ (member $ tailKey key) $ headKey key
    insert !key val
        = insert_ (insert (tailKey key) val) empty $ headKey key
    foldrWithKey = foldrWithKey3
    foldrWithKey' = foldrWithKey3'
    toList = toAscList3
    toAscList = toAscList3
    fromList = fromList3

instance (Enum a, Enum b) => KEMM (Key2 a b) v where
    type EnumMapMap (Key2 a b) v = EMM2 a b v
    type List (Key2 a b) v = [(a, [(b, v)])]
    type TailKey (Key2 a b) = Key1 b
    tailKey (Key2 _ k2) = Key1 k2
    type HeadKey (Key2 a b) = a
    headKey (Key2 k1 _) = fromEnum k1
    lookup !key
        = lookup_ (lookup $ tailKey key) $ headKey key
    member !key
        = member_ (member $ tailKey key) $ headKey key
    insert !key val
        = insert_ (insert (tailKey key) val) empty $ headKey key
    foldrWithKey = foldrWithKey2
    foldrWithKey' = foldrWithKey2'
    toList = toAscList2
    toAscList = toAscList2
    fromList = fromList2

instance (Enum a) => KEMM (Key1 a) v where
    type EnumMapMap (Key1 a) v = EMM1 a v
    type List (Key1 a) v = [(a, v)]
    type TailKey (Key1 a) = Key0
    tailKey (Key1 _) = Key0
    type HeadKey (Key1 a) = a
    headKey (Key1 k1) = fromEnum k1
    lookup !key = lookup_ Just $ headKey key
    member !key
        = member_ (\_ -> True) $ headKey key
    insert !key val
        = insert_ (\_ -> val) val $ headKey key
    foldrWithKey = foldrWithKey1
    foldrWithKey' = foldrWithKey1'
    toList = toAscList1
    toAscList = toAscList1
    fromList = fromList1

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
      Nothing -> error ("IntMap.find: key " ++ show key ++
                        " is not an element of the map")
      Just x  -> x

lookup_ :: (v -> Maybe t) -> Key -> EMM a v -> Maybe t
lookup_ f key (Bin _ m l r)
          | zero key m  = lookup_ f key l
          | otherwise = lookup_ f key r
lookup_ f key (Tip kx x)
          | key == kx = f x
          | otherwise = Nothing
lookup_ _ _ Nil = Nothing

member_ :: (v -> Bool ) -> Key -> EMM a v -> Bool
member_ f key t =
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

insert_ :: (v -> v) -> v -> Key -> EMM a v -> EMM a v
insert_ f def !key emm =
    case imm of
      Bin p m l r
          | nomatch key p m -> join key (Tip key def) p emm
          | zero key m      -> Bin p m (insert_ f def key l) r
          | otherwise       -> Bin p m l (insert_ f def key r)
      Tip ky t
          | key == ky         -> Tip key $ f t
          | otherwise         -> join key (Tip key def) ky emm
      Nil -> Tip key def

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

foldrWithKey1 :: (Enum a) =>
                 ((Key1 a) -> v -> t -> t)
              -> t -> EnumMapMap (Key1 a) v -> t
foldrWithKey1 f = foldrWithKey_ (\k1 -> f $ Key1 k1)
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
              _ -> go z t
    where
      go z' Nil           = z'
      go z' (Tip kx tx)   = f (toEnum kx) tx z
      go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldrWithKey_ #-}

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
              _ -> go z t
    where
      go !z' Nil           = z'
      go !z' (Tip kx tx)   = f (toEnum kx) tx z
      go !z' (Bin _ _ l r) = go (go z' r) l

{-# INLINE foldrWithKey_' #-}

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

toAscList4 :: (Enum a, Enum b, Enum c, Enum d) =>
              EnumMapMap (Key4 a b c d) v -> List (Key4 a b c d) v
toAscList4 = toAscList_ toAscList3
{-# INLINE toAscList4 #-}

toAscList3 :: (Enum a, Enum b, Enum c) =>
              EnumMapMap (Key3 a b c) v -> List (Key3 a b c) v
toAscList3 = toAscList_ toAscList2
{-# INLINE toAscList3 #-}

toAscList2 :: (Enum a, Enum b) =>
              EnumMapMap (Key2 a b) v -> List (Key2 a b) v
toAscList2 = toAscList_ toAscList1
{-# INLINE toAscList2 #-}

toAscList1 :: Enum a => EnumMapMap (Key1 a) v -> List (Key1 a) v
toAscList1 = toAscList_ id
{-# INLINE toAscList1 #-}

toAscList_ :: Enum k => (v -> t) -> EMM k v -> [(k, t)]
toAscList_ f = foldrWithKey_ (\key x xs -> (key, f x):xs) []
{-# INLINE toAscList_ #-}

fromList4 :: (Enum a, Enum b, Enum c, Enum d) =>
             List (Key4 a b c d) v -> EnumMapMap (Key4 a b c d) v
fromList4 = fromList_ fromList3
{-# INLINE fromList4 #-}

fromList3 :: (Enum a, Enum b, Enum c) =>
             List (Key3 a b c) v -> EnumMapMap (Key3 a b c) v
fromList3 = fromList_ fromList2
{-# INLINE fromList3 #-}

fromList2 :: (Enum a, Enum b) =>
             List (Key2 a b) v -> EnumMapMap (Key2 a b) v
fromList2 = fromList_ fromList1
{-# INLINE fromList2 #-}

fromList1 :: Enum a => List (Key1 a) v -> EnumMapMap (Key1 a) v
fromList1 = fromList_ id
{-# INLINE fromList1 #-}

fromList_ :: Enum k => (t -> v) -> [(k, t)] -> EMM k v
fromList_ f = foldlStrict ins empty
    where
      ins t (k1, x) = insert (Key1 k1) (f x) t
{-# INLINE fromList_ #-}

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Nat conversion
--------------------------------------------------------------------}

natFromInt :: Int -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

natFromEnum :: (Enum a) => a -> Nat
natFromEnum = natFromInt . fromEnum
{-# INLINE natFromEnum #-}

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

