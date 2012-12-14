{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances,
 GeneralizedNewtypeDeriving, MagicHash, MultiParamTypeClasses, TypeFamilies,
 TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMapSet
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Joachim Breitner 2011
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
-- Based on Data.IntSet.Base
--
-----------------------------------------------------------------------------

module Data.EnumMapSet.Base (
            EnumMapSet,
            S(..), (:&)(..),
            -- * Query
            null,
            size,
            member,
            lookup,
            -- * Construction
            empty,
            singleton,
            insert,
            insertSub,
            delete,
            -- * Combine
            union,
            difference,
            intersection,
            -- * Map
            map,
            -- * Folds
            foldr,
            -- * Lists
            toList,
            fromList,
            keys,
            -- * Min/Max
            findMin,
            minView,
            deleteFindMin,
            -- * Internals
            EMS(..),
            EnumMapMap(KSC),
            suffixBitMask,
            prefixBitMask,
            bitmapOf,
            prefixOf
) where

import           Prelude hiding (lookup,
                                 map,
                                 filter,
                                 foldr, foldl,
                                 null, init,
                                 head, tail)

import           Data.Bits
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           GHC.Exts (Word(..), Int(..))
import           GHC.Prim (indexInt8OffAddr#)
#include "MachDeps.h"

import           Data.EnumMapMap.Base ((:&)(..),
                                       IsKey,
                                       EnumMapMap,
                                       Prefix, Nat, Mask,
                                       branchMask, mask,
                                       intFromNat,
                                       shiftRL, shiftLL,
                                       nomatch, zero,
                                       shorter,
                                       foldlStrict)
import qualified Data.EnumMapMap.Base as EMM

type EnumMapSet k = EnumMapMap k ()

type BitMap = Word

-- | Keys are terminated with the 'S' type.
--
-- > singleKey :: S Int
-- > singleKey = S 5
--
newtype S k = S k
           deriving (Show, Eq)

-- This is used instead of @EMM k BitMap@ in order to unpack the 'BitMap' in
-- 'Tip'. Hopefully this will lead to much optimisation by GHC.
data EMS k = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask
                 !(EMS k) !(EMS k)
           | Tip {-# UNPACK #-} !Int {-# UNPACK #-} !BitMap
           | Nil
             deriving (Show)

instance (Enum k, Eq k) => IsKey (S k) where
    data EnumMapMap (S k) v = KSC (EMS k)

    emptySubTrees e@(KSC emm) =
        case emm of
          Nil -> False
          _   -> EMM.emptySubTrees_ e
    emptySubTrees_ (KSC emm) = go emm
        where
          go t = case t of
                   Bin _ _ l r -> go l || go r
                   Tip _ _     -> False
                   Nil         -> True

    removeEmpties = id

    unsafeJoinKey (KSC _) = undefined

    empty = KSC Nil

    null (KSC ems) = case ems of
                       Nil -> True
                       _   -> False

    size (KSC ems) = go ems
        where
          go (Bin _ _ l r) = go l + go r
          go (Tip _ bm)    = bitcount 0 bm
          go Nil           = 0

    foldrWithKey f init (KSC ems)
        = case ems of Bin _ m l r | m < 0 -> go (go init l) r
                                  | otherwise -> go (go init r) l
                      _          -> go init ems
          where
            go init' Nil           = init'
            go init' (Tip kx bm)   = foldrBits kx f' init' bm
            go init' (Bin _ _ l r) = go (go init' r) l
            f' !k t = f (S $ toEnum k) undefined t

    findMin (KSC ems) =
        case ems of
          Nil             -> error "findMin: no minimal element"
          Tip k bm        -> (S $ toEnum $ k + lowestBitSet bm, undefined)
          Bin _ m l r
              |   m < 0   -> go r
              | otherwise -> go l
        where go (Tip k bm)      = (S $ toEnum $ k + lowestBitSet bm, undefined)
              go (Bin _ _ l' _) = go l'
              go Nil            = error "findMin: Nil"

    minViewWithKey (KSC ems) =
        goat ems >>= (\(k, r) -> return ((S $ toEnum k, undefined), KSC r))
            where
              goat t =
                  case t of Nil                 -> Nothing
                            Bin p m l r | m < 0 ->
                                            case go r of
                                              (result, r') ->
                                                  Just (result, bin p m l r')
                            _                   -> Just (go t)
              go (Bin p m l r) = case go l of
                                   (result, l') -> (result, bin p m l' r)
              go (Tip kx bm) = case lowestBitSet bm of
                                 bi -> (kx + bi,
                                           tip kx (bm .&. complement
                                                          (bitmapOfSuffix bi)))
              go Nil = error "minView Nil"

    union (KSC ems1) (KSC ems2) = KSC $ go ems1 ems2
        where
          go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
              | shorter m1 m2  = union1
              | shorter m2 m1  = union2
              | p1 == p2       = Bin p1 m1 (go l1 l2) (go r1 r2)
              | otherwise      = join p1 t1 p2 t2
              where
                union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
                        | zero p2 m1        = Bin p1 m1 (go l1 t2) r1
                        | otherwise         = Bin p1 m1 l1 (go r1 t2)

                union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
                        | zero p1 m2        = Bin p2 m2 (go t1 l2) r2
                        | otherwise         = Bin p2 m2 l2 (go t1 r2)

          go t@(Bin _ _ _ _) (Tip kx bm) = insertBM kx bm t
          go t@(Bin _ _ _ _) Nil = t
          go (Tip kx bm) t = insertBM kx bm t
          go Nil t = t

    difference (KSC ems1) (KSC ems2) = KSC $ go ems1 ems2
        where
          go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
              | shorter m1 m2  = difference1
              | shorter m2 m1  = difference2
              | p1 == p2       = bin p1 m1 (go l1 l2) (go r1 r2)
              | otherwise      = t1
              where
                difference1 | nomatch p2 p1 m1  = t1
                            | zero p2 m1        = bin p1 m1 (go l1 t2) r1
                            | otherwise         = bin p1 m1 l1 (go r1 t2)

                difference2 | nomatch p1 p2 m2  = t1
                            | zero p1 m2        = go t1 l2
                            | otherwise         = go t1 r2

          go t@(Bin _ _ _ _) (Tip kx bm) = deleteBM kx bm t
          go t@(Bin _ _ _ _) Nil = t

          go t1@(Tip kx bm) t2 = differenceTip t2
              where differenceTip (Bin p2 m2 l2 r2)
                        | nomatch kx p2 m2 = t1
                        | zero kx m2 = differenceTip l2
                        | otherwise = differenceTip r2
                    differenceTip (Tip kx2 bm2)
                        | kx == kx2 = tip kx (bm .&. complement bm2)
                        | otherwise = t1
                    differenceTip Nil = t1

          go Nil _ = Nil

    intersection (KSC ems1) (KSC ems2) = KSC $ go ems1 ems2
        where
          go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
              | shorter m1 m2  = intersection1
              | shorter m2 m1  = intersection2
              | p1 == p2       = bin p1 m1 (go l1 l2) (go r1 r2)
              | otherwise      = Nil
              where
                intersection1 | nomatch p2 p1 m1  = Nil
                              | zero p2 m1        = go l1 t2
                              | otherwise         = go r1 t2

                intersection2 | nomatch p1 p2 m2  = Nil
                              | zero p1 m2        = go t1 l2
                              | otherwise         = go t1 r2

          go t1@(Bin _ _ _ _) (Tip kx2 bm2) = intersectBM t1
              where intersectBM (Bin p1 m1 l1 r1)
                        | nomatch kx2 p1 m1 = Nil
                        | zero kx2 m1       = intersectBM l1
                        | otherwise         = intersectBM r1
                    intersectBM (Tip kx1 bm1)
                        | kx1 == kx2 = tip kx1 (bm1 .&. bm2)
                        | otherwise = Nil
                    intersectBM Nil = Nil

          go (Bin _ _ _ _) Nil = Nil

          go (Tip kx1 bm1) t2 = intersectBM t2
              where intersectBM (Bin p2 m2 l2 r2)
                        | nomatch kx1 p2 m2 = Nil
                        | zero kx1 m2       = intersectBM l2
                        | otherwise         = intersectBM r2
                    intersectBM (Tip kx2 bm2)
                        | kx1 == kx2 = tip kx1 (bm1 .&. bm2)
                        | otherwise = Nil
                    intersectBM Nil = Nil

          go Nil _ = Nil

    equal (KSC ems1) (KSC ems2) = go ems1 ems2
        where
          go (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
              = (m1 == m2) && (p1 == p2) && (go l1 l2) && (go r1 r2)
          go (Tip kx1 bm1) (Tip kx2 bm2)
              = kx1 == kx2 && bm1 == bm2
          go Nil Nil = True
          go _   _   = False

    nequal (KSC ems1) (KSC ems2) = go ems1 ems2
        where
          go (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
              = (m1 /= m2) || (p1 /= p2) || (go l1 l2) || (go r1 r2)
          go (Tip kx1 bm1) (Tip kx2 bm2)
              = kx1 /= kx2 || bm1 /= bm2
          go Nil Nil = False
          go _   _   = True

    alter = undefined
    foldr = undefined
    map = undefined
    mapWithKey = undefined
    unionWith = undefined
    unionWithKey = undefined
    differenceWith = undefined
    differenceWithKey = undefined
    intersectionWith = undefined
    intersectionWithKey = undefined
    fromList = undefined
    toList = undefined
    elems = undefined
    keysSet = undefined
    fromSet = undefined

{---------------------------------------------------------------------
  Exported API

  The Set API is somewhat different to the Map API so we define the following
  functions to call the IsEMM functions with the type of 'v' as (), hoping that
  GHC will inline away all the empty parameters.
---------------------------------------------------------------------}

null :: (IsKey k) => EnumMapSet k -> Bool
null = EMM.null

size :: (IsKey k) => EnumMapSet k -> Int
size = EMM.size

member ::(EMM.SubKey k1 k2 (), IsKey k1, IsKey k2) =>
         k1 -> EnumMapSet k2 -> Bool
member = EMM.member

-- | Lookup a subtree in an 'EnumMapSet'.
--
-- > ems = fromList [1 :& 2 :& K 3, 1 :& 2 :& K 4]
-- > lookup (1 :& K 2) ems == fromList [K 3, K 4]
-- > lookup (1 :& 2 :& K 3) -- ERROR: Use 'member' to check for a key.
--
lookup :: (EMM.SubKey k1 k2 (), IsKey k1, IsKey k2) =>
          k1 -> EnumMapSet k2 -> Maybe (EMM.Result k1 k2 ())
lookup = EMM.lookup

empty :: (IsKey k) => EnumMapSet k
empty = EMM.empty

singleton :: (IsKey k, EMM.SubKey k k (), EMM.Result k k () ~ ()) =>
             k -> EnumMapSet k
singleton !key = EMM.singleton key ()

insert :: (IsKey k, EMM.SubKey k k (), EMM.Result k k () ~ ()) =>
          k -> EnumMapSet k -> EnumMapSet k
insert !key = EMM.insert key ()

insertSub :: (IsKey k1, IsKey k2, EMM.SubKey k1 k2 ()) =>
             k1 -> EMM.Result k1 k2 () -> EnumMapSet k2 -> EnumMapSet k2
insertSub !key = EMM.insert key

delete :: (EMM.SubKey k1 k2 (), IsKey k1, IsKey k2) =>
          k1 -> EnumMapSet k2 -> EnumMapSet k2
delete = EMM.delete

-- This function has not been optimised in any way.
foldr :: (IsKey k) => (k -> t -> t) -> t -> EnumMapSet k -> t
foldr f = EMM.foldrWithKey go
    where
      go k _ z = f k z

-- | @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: (IsKey k1, IsKey k2, EMM.SubKey k2 k2 (), EMM.Result k2 k2 () ~ ()) =>
       (k1 -> k2) -> EnumMapSet k1 -> EnumMapSet k2
map f = fromList . List.map f . toList

findMin :: (IsKey k) => EnumMapSet k -> k
findMin = fst . EMM.findMin

minView :: (IsKey k) => EnumMapSet k -> Maybe (k, EnumMapSet k)
minView ems = EMM.minViewWithKey ems >>= \((k, _), ems') -> return (k, ems')

deleteFindMin :: (IsKey k) => EnumMapSet k -> (k, EnumMapSet k)
deleteFindMin =
    fromMaybe (error "deleteFindMin: empty EnumMapSet has no minimal element")
                  . minView

union :: (IsKey k) => EnumMapSet k -> EnumMapSet k -> EnumMapSet k
union = EMM.union

difference :: (IsKey k) => EnumMapSet k -> EnumMapSet k -> EnumMapSet k
difference = EMM.difference

intersection :: (IsKey k) => EnumMapSet k -> EnumMapSet k -> EnumMapSet k
intersection = EMM.intersection

{---------------------------------------------------------------------
  Lists
---------------------------------------------------------------------}

fromList :: (IsKey k, EMM.SubKey k k (), EMM.Result k k () ~ ()) =>
            [k] -> EnumMapSet k
fromList xs
    = foldlStrict (\t x -> insert x t) empty xs

toList :: IsKey k => EnumMapSet k -> [k]
toList = foldr (:) []

keys :: IsKey k => EnumMapSet k -> [k]
keys = toList

{---------------------------------------------------------------------
  Instances
---------------------------------------------------------------------}

instance EMM.HasSKey (S k) where
    type Skey (S k) = S k
    toS (S _) = undefined
    toK (S _) = undefined

instance (Enum k1, k1 ~ k2) => EMM.SubKey (S k1) (k2 :& t2) () where
    type Result (S k1) (k2 :& t2) () = EnumMapSet t2

    member !(S key') (EMM.KCC emm) = key `seq` go emm
        where
          go t = case t of
               EMM.Bin _ m l r -> case zero key m of
                                    True  -> go l
                                    False -> go r
               EMM.Tip kx _    -> key == kx
               EMM.Nil         -> False
          key = fromEnum key'

    singleton !(S key) = EMM.KCC . EMM.Tip (fromEnum key)

    lookup (S key') (EMM.KCC emm) = key `seq` go emm
        where
          go (EMM.Bin _ m l r)
             | zero key m = go l
             | otherwise = go r
          go (EMM.Tip kx x)
             = case kx == key of
                 True -> Just x
                 False -> Nothing
          go EMM.Nil = Nothing
          key = fromEnum key'

    insert (S key') val (EMM.KCC emm) = key `seq` EMM.KCC $ go emm
        where
          go t =
              case t of
                EMM.Bin p m l r
                    | nomatch key p m -> EMM.join key (EMM.Tip key val) p t
                    | zero key m      -> EMM.Bin p m (go l) r
                    | otherwise       -> EMM.Bin p m l (go r)
                EMM.Tip ky _
                    | key == ky       -> EMM.Tip key val
                    | otherwise       -> EMM.join key (EMM.Tip key val) ky t
                EMM.Nil               -> EMM.Tip key val
          key = fromEnum key'

    delete (S key') (EMM.KCC emm) = key `seq` EMM.KCC $ go emm
        where
          go t = case t of
                   EMM.Bin p m l r | nomatch key p m -> t
                                   | zero key m      -> EMM.bin p m (go l) r
                                   | otherwise       -> EMM.bin p m l (go r)
                   EMM.Tip ky _    | key == ky       -> EMM.Nil
                                   | otherwise       -> t
                   EMM.Nil                           -> EMM.Nil
          key = fromEnum key'

    insertWith = undefined
    insertWithKey = undefined

instance (Enum k) => EMM.SubKey (S k) (S k) () where
    type Result (S k) (S k) () = ()
    member !(S key') (KSC ems) = key `seq` go ems
        where
          go (Bin p m l r)
              | nomatch key p m = False
              | zero key m      = go l
              | otherwise       = go r
          go (Tip y bm) = prefixOf key == y && bitmapOf key .&. bm /= 0
          go Nil = False
          key = fromEnum key'
    singleton !(S key') _ = KSC $! Tip (prefixOf key) (bitmapOf key)
          where key = fromEnum key'
    lookup = undefined
    insert (S key') _ (KSC ems) =
        key `seq` KSC $ insertBM (prefixOf key) (bitmapOf key) ems
            where key = fromEnum key'
    delete !(S key') (KSC ems) =
        key `seq` KSC $ deleteBM (prefixOf key) (bitmapOf key) ems
          where key = fromEnum key'

    insertWith = undefined
    insertWithKey = undefined

instance (Show v) => Show (EnumMapMap (S k) v) where
    show (KSC ems) = show ems

{---------------------------------------------------------------------
  Helper functions
---------------------------------------------------------------------}

insertBM :: Prefix -> BitMap -> EMS k -> EMS k
insertBM !kx !bm t =
    case t of
      Bin p m l r
          | nomatch kx p m -> join kx (Tip kx bm) p t
          | zero kx m      -> Bin p m (insertBM kx bm l) r
          | otherwise      -> Bin p m l (insertBM kx bm r)
      Tip kx' bm'
          | kx' == kx -> Tip kx' (bm .|. bm')
          | otherwise -> join kx (Tip kx bm) kx' t
      Nil -> Tip kx bm

deleteBM :: Prefix -> BitMap -> EMS k -> EMS k
deleteBM !kx !bm t
  = case t of
      Bin p m l r
          | nomatch kx p m -> t
          | zero kx m      -> bin p m (deleteBM kx bm l) r
          | otherwise      -> bin p m l (deleteBM kx bm r)
      Tip kx' bm'
          | kx' == kx -> tip kx (bm' .&. complement bm)
          | otherwise -> t
      Nil -> Nil

join :: Prefix -> EMS k -> Prefix -> EMS k -> EMS k
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE join #-}

bin :: Prefix -> Mask -> EMS k -> EMS k -> EMS k
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

{--------------------------------------------------------------------
  @tip@ assures that we never have empty bitmaps within a tree.
--------------------------------------------------------------------}
tip :: Prefix -> BitMap -> EMS k
tip _ 0 = Nil
tip kx bm = Tip kx bm
{-# INLINE tip #-}

{----------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.

  Commentary and credits can be found with the original code in
  Data/IntSet/Base.hs in 'containers 5.0'.
----------------------------------------------------------------------}

suffixBitMask :: Int
suffixBitMask = bitSize (undefined::Word) - 1
{-# INLINE suffixBitMask #-}

prefixBitMask :: Int
prefixBitMask = complement suffixBitMask
{-# INLINE prefixBitMask #-}

prefixOf :: Int -> Prefix
prefixOf x = x .&. prefixBitMask
{-# INLINE prefixOf #-}

suffixOf :: Int -> Int
suffixOf x = x .&. suffixBitMask
{-# INLINE suffixOf #-}

bitmapOfSuffix :: Int -> BitMap
bitmapOfSuffix s = 1 `shiftLL` s
{-# INLINE bitmapOfSuffix #-}

bitmapOf :: Int -> BitMap
bitmapOf x = bitmapOfSuffix (suffixOf x)
{-# INLINE bitmapOf #-}

bitcount :: Int -> Word -> Int
bitcount a0 x0 = go a0 x0
  where go a 0 = a
        go a x = go (a + 1) (x .&. (x-1))
{-# INLINE bitcount #-}

{----------------------------------------------------------------------
  Folds over a BitMap.

  Commentary and credits can be found with the original code in
  Data/IntSet/Base.hs in 'containers 5.0'.
----------------------------------------------------------------------}

foldrBits :: Int -> (Int -> a -> a) -> a -> Nat -> a

{-# INLINE foldrBits #-}

indexOfTheOnlyBit :: Nat -> Int
{-# INLINE indexOfTheOnlyBit #-}
indexOfTheOnlyBit bitmask =
  I# (lsbArray `indexInt8OffAddr#` unboxInt
                   (intFromNat ((bitmask * magic) `shiftRL` offset)))
  where unboxInt (I# i) = i
#if WORD_SIZE_IN_BITS==32
        magic = 0x077CB531
        offset = 27
        !lsbArray = "\0\1\28\2\29\14\24\3\30\22\20\15\25\17\4\8\31\27\13\23\21\19\16\7\26\12\18\6\11\5\10\9"#
#else
        magic = 0x07EDD5E59A4E28C2
        offset = 58
        !lsbArray = "\63\0\58\1\59\47\53\2\60\39\48\27\54\33\42\3\61\51\37\40\49\18\28\20\55\30\34\11\43\14\22\4\62\57\46\52\38\26\32\41\50\36\17\19\29\10\13\21\56\45\25\31\35\16\9\12\44\24\15\8\23\7\6\5"#
#endif
lowestBitMask :: Nat -> Nat
lowestBitMask x = x .&. negate x
{-# INLINE lowestBitMask #-}

revNat :: Nat -> Nat
#if WORD_SIZE_IN_BITS==32
revNat x1 = case ((x1 `shiftRL` 1) .&. 0x55555555) .|. ((x1 .&. 0x55555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x33333333) .|. ((x2 .&. 0x33333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF) .|. ((x4 .&. 0x00FF00FF) `shiftLL` 8) of
                     x5 -> ( x5 `shiftRL` 16             ) .|. ( x5               `shiftLL` 16);
#else
revNat x1 = case ((x1 `shiftRL` 1) .&. 0x5555555555555555) .|. ((x1 .&. 0x5555555555555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x3333333333333333) .|. ((x2 .&. 0x3333333333333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF00FF00FF) .|. ((x4 .&. 0x00FF00FF00FF00FF) `shiftLL` 8) of
                     x5 -> case ((x5 `shiftRL` 16) .&. 0x0000FFFF0000FFFF) .|. ((x5 .&. 0x0000FFFF0000FFFF) `shiftLL` 16) of
                       x6 -> ( x6 `shiftRL` 32             ) .|. ( x6               `shiftLL` 32);
#endif
foldrBits prefix f z bitmap = go (revNat bitmap) z
  where go bm acc | bm == 0 = acc
                  | otherwise = case lowestBitMask bm of
                                  bitmask -> bitmask `seq` case indexOfTheOnlyBit bitmask of
                                    bi -> bi `seq` go (bm `xor` bitmask) ((f $! (prefix+(WORD_SIZE_IN_BITS-1)-bi)) acc)

lowestBitSet :: Nat -> Int
lowestBitSet x = indexOfTheOnlyBit (lowestBitMask x)
{-# INLINE lowestBitSet #-}
