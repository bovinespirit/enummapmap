{-# LANGUAGE MagicHash, TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}

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

module Data.EnumMapMap.Base2 (
            -- * Map type

) where

import           Prelude hiding (lookup,map,filter,foldr,foldl,null)

import           Control.Applicative (Applicative(pure,(<*>)),(<$>))
import           Control.DeepSeq (NFData(rnf))
import           Data.Bits
import qualified Data.Foldable as Foldable
import           Data.Monoid (Monoid(..))
import           Data.Traversable (Traversable(traverse))
import           GHC.Exts (Word(..), Int(..), shiftRL#, Constraint)

data EMM k v = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask
                     !(EMM k v) !(EMM k v)
             | Tip {-# UNPACK #-} !Int v
             | Nil
               deriving (Show)

type Nat    = Word
type Key    = Int
type Prefix = Int
type Mask   = Int

data KeyList k n = KeyList k n
    deriving (Show)
data KeyEnd k = KeyEnd k
    deriving (Show)

type K1 a       =                                  KeyEnd a
type K2 a b     =                       KeyList a (KeyEnd b)
type K3 a b c   =            KeyList a (KeyList b (KeyEnd c))
type K4 a b c d = KeyList a (KeyList b (KeyList c (KeyEnd d)))

newtype EnumMapMap4 a b c d v = EMM4 {unEMM4 :: EMM a (EMM b (EMM c (EMM d v)))}
newtype EnumMapMap3   b c d v = EMM3 {unEMM3 ::        EMM b (EMM c (EMM d v)) }
newtype EnumMapMap2     c d v = EMM2 {unEMM2 ::               EMM c (EMM d v)  }
newtype EnumMapMap1       d v = EMM1 {unEMM1 ::                      EMM d v   }

class UnEmm e where
    type Tail e :: *
    type Head e :: *
    unEMM :: e -> EMM (Head e) (Tail e)
    mkEMM :: EMM (Head e) (Tail e) -> e

instance UnEmm (EnumMapMap4 a b c d v) where
    type Tail (EnumMapMap4 a b c d v) = EMM b (EMM c (EMM d v))
    type Head (EnumMapMap4 a b c d v) = a
    unEMM = unEMM4
    mkEMM = EMM4

instance UnEmm (EnumMapMap3 a b c v) where
    type Tail (EnumMapMap3 a b c v) = EMM b (EMM c v)
    type Head (EnumMapMap3 a b c v) = a
    unEMM = unEMM3
    mkEMM = EMM3

instance UnEmm (EnumMapMap2 a b v) where
    type Tail (EnumMapMap2 a b v) = EMM b v
    type Head (EnumMapMap2 a b v) = a
    unEMM = unEMM2
    mkEMM = EMM2

instance UnEmm (EnumMapMap1 a v) where
    type Tail (EnumMapMap1 a v) = v
    type Head (EnumMapMap1 a v) = a
    unEMM = unEMM1
    mkEMM = EMM1

class EmmKey k where
    type EmmC k :: * -> *
    type EmmCons k :: Constraint
    type EmmCons k = ()
    type HeadKey k :: *
    type TailKey k :: *
    headKey :: k -> Key
    tailKey :: k -> TailKey k
    lookup' :: EmmCons k => k -> EmmC k v -> Maybe t

instance (Enum a) => EmmKey (KeyEnd a) where
    type EmmC (KeyEnd a) = EMM a
    type HeadKey (KeyEnd a) = a
    type TailKey (KeyEnd a) = ()
    headKey (KeyEnd k) = fromEnum k
    tailKey = undefined
    lookup' k = lookup_ Just (headKey k)

instance (Enum a, EmmKey n) => EmmKey (KeyList a n) where
    type EmmC (KeyList a n) = EMM a
    type EmmCons (KeyList a n) = EmmC n ~ EMM (HeadKey n)
    type TailKey (KeyList a n) = n
    type HeadKey (KeyList a n) = a
    headKey (KeyList key _) = fromEnum key
    tailKey (KeyList _ tail) = tail
    lookup' k = lookup_ (lookup' $ tailKey k) (headKey k)

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

null :: UnEmm e => e -> Bool
null t = case unEMM t of
           Nil -> True
           _   -> False

size :: UnEmm e => e -> Int
size = go . unEMM
    where go t = case t of
                   Bin _ _ l r -> go l + go r
                   Tip _ _     -> 1
                   Nil         -> 0

empty :: UnEmm e => e
empty = mkEMM Nil

lookup :: (EmmKey k, UnEmm e) => k -> e -> Maybe v
lookup key emm = lookup' key $ unEMM emm

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
tip k val = Tip k val
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

