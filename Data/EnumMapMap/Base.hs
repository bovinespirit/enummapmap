{-# LANGUAGE MagicHash, TypeFamilies, MultiParamTypeClasses,
             BangPatterns, FlexibleInstances #-}

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
            K1, K2, K3, K4,
            k1, k2, k3, k4,
            EnumMapMap,
            -- * Query
            size,
            null,
            member,
            lookup,
            -- * Construction
            empty,
            singleton,
            -- * Insertion
            insert,
            insertWithKey,
            -- * Delete\/Update
            delete,
            -- * Combine
            -- ** Union
            union,
            unionWith,
            unionWithKey,
            unions,
            -- ** Difference
            difference,
            differenceWith,
            differenceWithKey,
            -- ** Intersection
            intersection,
            intersectionWith,
            intersectionWithKey,
            -- * Traversal
            -- ** Map
            map,
            mapWithKey,
            -- * Folds
            foldrWithKey,
            -- * Lists
            toList,
            fromList
) where

import           Prelude hiding (lookup,map,filter,foldr,foldl,null, init)

import           Data.Bits
import           Data.Monoid (Monoid(..))
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

-- | Keys are made up of a list of 'Enums'.
data KeyCons k t = KeyCons !k !t
                   deriving (Show, Eq)
data KeyEnd k = KeyEnd !k
                   deriving (Show, Eq)

type K1 a       = KeyEnd a
type K2 a b     = KeyCons a (K1 b)
type K3 a b c   = KeyCons a (K2 b c)
type K4 a b c d = KeyCons a (K3 b c d)

k1 :: a -> K1 a
k1 a       = KeyEnd a
k2 :: a -> b -> K2 a b
k2 a b     = KeyCons a $ k1 b
k3 :: a -> b -> c -> K3 a b c
k3 a b c   = KeyCons a $ k2 b c
k4 :: a -> b -> c -> d -> K4 a b c d
k4 a b c d = KeyCons a $ k3 b c d

class IsEmm k where
    data EnumMapMap k :: * -> *

    empty :: EnumMapMap k v
    null :: EnumMapMap k v -> Bool
    size :: EnumMapMap k v -> Int
    member :: k -> EnumMapMap k v -> Bool
    singleton :: k -> v -> EnumMapMap k v
    lookup :: k -> EnumMapMap k v -> Maybe v
    insert :: k -> v -> EnumMapMap k v -> EnumMapMap k v
    insertWithKey :: (k -> v -> v -> v)
                  -> k -> v -> EnumMapMap k v -> EnumMapMap k v
    delete :: k -> EnumMapMap k v -> EnumMapMap k v
    map :: (v -> t) -> EnumMapMap k v -> EnumMapMap k t
    map f = mapWithKey (\_ -> f)
    mapWithKey :: (k -> v -> t) -> EnumMapMap k v -> EnumMapMap k t
    foldrWithKey :: (k -> v -> t -> t) -> t -> EnumMapMap k v -> t
    toList :: EnumMapMap k v -> [(k, v)]
    toList = foldrWithKey (\k x xs -> (k, x):xs) []
    fromList :: [(k, v)] -> EnumMapMap k v
    fromList = foldlStrict (\t (k, x) -> insert k x t) empty
    union :: EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    unions :: [EnumMapMap k v] -> EnumMapMap k v
    unions = foldlStrict union empty
    unionWith :: (v -> v -> v)
              -> EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    unionWith f = unionWithKey (\_ -> f)
    unionWithKey :: (k -> v -> v -> v)
                 -> EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    difference ::  EnumMapMap k v1 -> EnumMapMap k v2 -> EnumMapMap k v1
    differenceWith :: (v1 -> v2 -> Maybe v1)
                   -> EnumMapMap k v1
                   -> EnumMapMap k v2
                   -> EnumMapMap k v1
    differenceWith f = differenceWithKey (\_ -> f)
    differenceWithKey :: (k -> v1 -> v2 -> Maybe v1)
                      -> EnumMapMap k v1
                      -> EnumMapMap k v2
                      -> EnumMapMap k v1
    intersection :: EnumMapMap k v1
                 -> EnumMapMap k v2
                 -> EnumMapMap k v1
    intersectionWith :: (v1 -> v2 -> v3)
                     -> EnumMapMap k v1
                     -> EnumMapMap k v2
                     -> EnumMapMap k v3
    intersectionWith f = intersectionWithKey (\_ -> f)
    intersectionWithKey :: (k -> v1 -> v2 -> v3)
                        -> EnumMapMap k v1
                        -> EnumMapMap k v2
                        -> EnumMapMap k v3

    equal ::  Eq v => EnumMapMap k v -> EnumMapMap k v -> Bool
    nequal :: Eq v => EnumMapMap k v -> EnumMapMap k v -> Bool

instance (Enum k) => IsEmm (KeyEnd k) where
    data EnumMapMap (KeyEnd k) v = KEC (EMM k v)

    empty = KEC Nil

    null (KEC t) = case t of
                     Nil -> True
                     _   -> False

    size (KEC t) = go t
        where
          go (Bin _ _ l r) = go l + go r
          go (Tip _ _)     = 1
          go Nil           = 0

    member !(KeyEnd key') (KEC emm) = go emm
        where
          go t = case t of
                   Bin _ m l r -> case zero key m of
                                    True  -> go l
                                    False -> go r
                   Tip kx _    -> key == kx
                   Nil         -> False
          key = fromEnum key'

    singleton !(KeyEnd key) = KEC . Tip (fromEnum key)

    lookup !(KeyEnd key') (KEC emm) = go emm
        where
          go (Bin _ m l r)
              | zero key m = go l
              | otherwise = go r
          go (Tip kx x)
             = case kx == key of
                 True -> Just x
                 False -> Nothing
          go Nil = Nothing
          key = fromEnum key'

    insert !(KeyEnd key') val (KEC emm) = KEC $ go emm
        where
          go t = case t of
                   Bin p m l r
                       | nomatch key p m -> join key (Tip key val) p t
                       | zero key m      -> Bin p m (go l) r
                       | otherwise       -> Bin p m l (go r)
                   Tip ky _
                       | key == ky       -> Tip key val
                       | otherwise       -> join key (Tip key val) ky t
                   Nil                   -> Tip key val
          key = fromEnum key'

    insertWithKey f k@(KeyEnd key') val (KEC emm) = KEC $ go emm
        where go t = case t of
                     Bin p m l r
                         | nomatch key p m -> join key (Tip key val) p t
                         | zero key m      -> Bin p m (go l) r
                         | otherwise       -> Bin p m l (go r)
                     Tip ky y
                         | key == ky       -> Tip key (f k val y)
                         | otherwise       -> join key (Tip key val) ky t
                     Nil                   -> Tip key val
              key = fromEnum key'

    delete !(KeyEnd key') (KEC emm) = KEC $ go emm
        where
          go t = case t of
                   Bin p m l r | nomatch key p m -> t
                               | zero key m      -> bin p m (go l) r
                               | otherwise       -> bin p m l (go r)
                   Tip ky _    | key == ky       -> Nil
                               | otherwise       -> t
                   Nil                           -> Nil
          key = fromEnum key'

    mapWithKey f (KEC emm) = KEC $ mapWithKey_ (\k -> f $ KeyEnd k) emm
    foldrWithKey f init (KEC emm) = foldrWithKey_ (\k -> f $ KeyEnd k) init emm

    union (KEC emm1) (KEC emm2) = KEC $ mergeWithKey' Bin const id id emm1 emm2
    unionWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' Bin go id id emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ f (KeyEnd $ toEnum k1) x1 x2

    difference (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin (\_ _ -> Nil) id (const Nil) emm1 emm2
    differenceWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin combine id (const Nil) emm1 emm2
            where
              combine = \(Tip k1 x1) (Tip _ x2)
                      -> case f (KeyEnd $ toEnum k1) x1 x2 of
                           Nothing -> Nil
                           Just x -> Tip k1 x

    intersection (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin const (const Nil) (const Nil) emm1 emm2
    intersectionWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin go (const Nil) (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ f (KeyEnd $ toEnum k1) x1 x2

    equal (KEC emm1) (KEC emm2) = emm1 == emm2
    nequal (KEC emm1) (KEC emm2) = emm1 /= emm2

instance (Enum k, IsEmm t) => IsEmm (KeyCons k t) where
    data EnumMapMap (KeyCons k t) v = KCC (EMM k (EnumMapMap t v))

    empty = KCC Nil

    null (KCC t) =
        case t of
          Nil -> True
          _   -> False

    size (KCC t) = go t
        where
          go (Bin _ _ l r) = go l + go r
          go (Tip _ y)     = size y
          go Nil           = 0

    member !(KeyCons key' nxt) (KCC emm) = go emm
        where
          go t = case t of
                   Bin _ m l r -> case zero key m of
                                    True  -> go l
                                    False -> go r
                   Tip kx x    -> case key == kx of
                                    True  -> member nxt x
                                    False -> False
                   Nil         -> False
          key = fromEnum key'

    singleton (KeyCons key nxt) = KCC . Tip (fromEnum key) . singleton nxt

    lookup (KeyCons key nxt) (KCC emm) = go emm
        where
          go (Bin _ m l r)
              | zero (fromEnum key) m = go l
              | otherwise = go r
          go (Tip kx x)
             = case kx == (fromEnum key) of
                 True -> lookup nxt x
                 False -> Nothing
          go Nil = Nothing

    insert (KeyCons key nxt) val (KCC emm)
        = KCC $ insertWith_ (insert nxt val) key (singleton nxt val) emm

    insertWithKey f k@(KeyCons key nxt) val (KCC emm) =
        KCC $ insertWith_ go key (singleton nxt val) emm
            where
              go = insertWithKey (\_ -> f k) nxt val

    delete !(KeyCons key nxt) (KCC emm) =
        KCC $ alter_ (delete nxt) (fromEnum key) emm

    mapWithKey f (KCC emm) = KCC $ mapWithKey_ go emm
        where
          go k = mapWithKey (\nxt -> f $ KeyCons k nxt)

    foldrWithKey f init (KCC emm) = foldrWithKey_ go init emm
        where
          go k val z = foldrWithKey (\nxt -> f $ KeyCons k nxt) z val

    union (KCC emm1) (KCC emm2) = KCC $ mergeWithKey' binD go id id emm1 emm2
        where
          go = \(Tip k1 x1) (Tip _ x2) -> tip k1 $ union x1 x2
    unionWithKey f (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go id id emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ unionWithKey (g k1) x1 x2
              g k1 nxt = f $ KeyCons (toEnum k1) nxt

    difference (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go id (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   tip k1 (difference x1 x2)
    differenceWithKey f (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go id (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   tip k1 $ differenceWithKey (\nxt ->
                                              f $ KeyCons (toEnum k1) nxt) x1 x2

    intersection (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go (const Nil) (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ intersection x1 x2
    intersectionWithKey f (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go (const Nil) (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ intersectionWithKey (\nxt ->
                                                f $ KeyCons (toEnum k1) nxt) x1 x2

    equal (KCC emm1) (KCC emm2) = emm1 == emm2
    nequal (KCC emm1) (KCC emm2) = emm1 /= emm2

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

insertWith_ :: Enum k => (v -> v) -> k -> v -> EMM k v -> EMM k v
insertWith_ f !key' val emm = key `seq` go emm
    where
      go t =
          case t of
            Bin p m l r
                | nomatch key p m -> join key (Tip key val) p t
                | zero key m      -> Bin p m (go l) r
                | otherwise       -> Bin p m l (go r)
            Tip ky y
                | key == ky       -> Tip key (f y)
                | otherwise       -> join key (Tip key val) ky t
            Nil                   -> Tip key val
      key = fromEnum key'
{-# INLINE insertWith_ #-}

-- 'alter_' is used to walk down the tree to find the EnumMapMap to actually
-- change.  If the new EnumMapMap is null then it's removed from the containing
-- EMM.
alter_ :: (IsEmm b) =>
          (EnumMapMap b v -> EnumMapMap b v)
       -> Key
       -> EMM a (EnumMapMap b v)
       -> EMM a (EnumMapMap b v)
alter_ f k = go
    where
      go t =
          case t of
            Bin p m l r | nomatch k p m -> t
                        | zero k m      -> binD p m (go l) r
                        | otherwise     -> binD p m l (go r)
            Tip ky y    | k == ky       -> tip k $ f y
                        | otherwise     -> t
            Nil                         -> Nil

mapWithKey_ :: Enum k => (k -> v -> t) -> EMM k v -> EMM k t
mapWithKey_ f = go
    where
      go (Bin p m l r) = Bin p m (go l) (go r)
      go (Tip k x)     = Tip k (f (toEnum k) x)
      go Nil           = Nil
{-# INLINE mapWithKey_ #-}

foldrWithKey_ :: (Enum k) => (k -> v -> t -> t) -> t -> EMM k v -> t
foldrWithKey_ f z = \emm ->
    case emm of Bin _ m l r | m < 0     -> go (go z l) r
                            | otherwise -> go (go z r) l
                _                       -> go z emm
    where
      go z' Nil           = z'
      go z' (Tip kx tx)   = f (toEnum kx) tx z'
      go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldrWithKey_ #-}

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


{---------------------------------------------------------------------
 Instances
---------------------------------------------------------------------}

-- Eq

instance (Eq v, IsEmm k) => Eq (EnumMapMap k v) where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

instance Eq v => Eq (EMM k v) where
  t1 == t2  = equalE t1 t2
  t1 /= t2  = nequalE t1 t2

equalE :: Eq v => EMM k v -> EMM k v -> Bool
equalE (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equalE l1 l2) && (equalE r1 r2)
equalE (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equalE Nil Nil = True
equalE _   _   = False

nequalE :: Eq v => EMM k v -> EMM k v -> Bool
nequalE (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequalE l1 l2) || (nequalE r1 r2)
nequalE (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequalE Nil Nil = False
nequalE _   _   = True

instance (IsEmm k) => Functor (EnumMapMap k)
    where
      fmap = map

instance (IsEmm k) => Monoid (EnumMapMap k v) where
    mempty = empty
    mappend = union
    mconcat = unions

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
binD :: (IsEmm b) =>
        Prefix -> Mask
     -> EMM a (EnumMapMap b v)
     -> EMM a (EnumMapMap b v)
     -> EMM a (EnumMapMap b v)
binD _ _ l Nil = l
binD _ _ Nil r = r
binD p m l r@(Tip _ y)
    | null y    = l
    | otherwise = Bin p m l r
binD p m l@(Tip _ y) r
    | null y    = r
    | otherwise = Bin p m l r
binD p m l r = Bin p m l r
{-# INLINE binD #-}

tip :: (IsEmm b) => Key -> EnumMapMap b v -> EMM a (EnumMapMap b v)
tip k val
    | null val  = Nil
    | otherwise = Tip k val
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

