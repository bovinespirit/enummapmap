{-# LANGUAGE CPP, BangPatterns, FlexibleInstances, GeneralizedNewtypeDeriving,
  MagicHash, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMapMap.Strict
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
-- Strict 'EnumMapMap'.  Based upon "Data.IntMap.Strict", this version uses multi
-- dimensional keys and 'Enum' types instead of 'Int's.  Keys are built using
-- the ':&' operator and terminated with 'K'.  They are stored using 'Int's so 2
-- keys that 'Enum' to the same 'Int' value will overwrite each other.  The
-- intension is that the 'Enum' types will actually be @newtype 'Int'@s.
--
-- > newtype AppleID = AppleID Int
-- > newtype TreeID = TreeID Int
-- > type Orchard = EnumMapMap (TreeID :& K AppleID) Apple
-- > apple = lookup (TreeID 4 :& K AppleID 32) orchard
--
-- The 'K' type is different to that used in "Data.EnumMapMap.Lazy" so only strict
-- operations can be performed on a strict 'EnumMapMap'.
--
-- The functions are strict on values and keys.
-----------------------------------------------------------------------------

module Data.EnumMapMap.Strict (

            emptySubTrees,

            -- * Key types
            (:&)(..), K(..),
            d1, d2, d3, d4, d5, d6, d7, d8, d9, d10,
            -- * Map Type
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
            insertWith,
            insertWithKey,
            -- * Delete\/Update
            delete,
            alter,
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
            differenceSet,
            -- ** Intersection
            intersection,
            intersectionWith,
            intersectionWithKey,
            intersectSet,
            -- * Map
            map,
            mapWithKey,
            -- * Folds
            foldr,
            foldrWithKey,
            -- * Lists and Sets
            toList,
            fromList,
            keys,
            elems,
            keysSet,
            fromSet,
            -- * Min/Max
            findMin,
            minViewWithKey,
            deleteFindMin,
            -- * Split/Join Keys
            toK,
            toS,
            splitKey,
            joinKey,
            unsafeJoinKey
) where

import           Prelude hiding (lookup,map,filter,foldr,foldl,null, init)

import           Control.DeepSeq (NFData(rnf))
import           Data.Bits

import           Data.EnumMapMap.Base
import qualified Data.EnumMapSet.Base as EMS

-- | Keys are terminated with the 'K' type
--
-- > singleKey :: K Int
-- > singleKey = K 5
--
newtype K k = K k
           deriving (Show, Eq)

instance (Enum k, Eq k) => IsKey (K k) where
    newtype EnumMapMap (K k) v = KEC (EMM k v)

    emptySubTrees e@(KEC emm) =
        case emm of
          Nil -> False
          _   -> emptySubTrees_ e
    emptySubTrees_ (KEC emm) = go emm
        where
          go t = case t of
                   Bin _ _ l r -> go l || go r
                   Tip _ _     -> False
                   Nil         -> True

    removeEmpties = id

    unsafeJoinKey (KEC emm) = KCC emm

    empty = KEC Nil

    null (KEC t) = case t of
                     Nil -> True
                     _   -> False

    size (KEC t) = go t
        where
          go (Bin _ _ l r) = go l + go r
          go (Tip _ _)     = 1
          go Nil           = 0

    alter f !(K key') (KEC emm) = KEC $ go emm
        where
          go t = case t of
                Bin p m l r
                    | nomatch key p m -> case f Nothing of
                                           Nothing -> t
                                           Just !x  -> join key (Tip key x) p t
                    | zero key m      -> bin p m (go l) r
                    | otherwise       -> bin p m l (go r)
                Tip ky y
                    | key == ky       -> case f (Just y) of
                                           Just !x  -> Tip ky x
                                           Nothing -> Nil
                    | otherwise       -> case f Nothing of
                                           Just !x  -> join key (Tip key x) ky t
                                           Nothing -> Tip ky y
                Nil                   -> case f Nothing of
                                           Just !x  -> Tip key x
                                           Nothing -> Nil
            where
              key = fromEnum key'

    mapWithKey f (KEC emm) = KEC $ mapWithKey_ (\k -> id $! f $! K k) emm
    foldr f init (KEC emm) =
        case emm of Bin _ m l r | m < 0 -> go (go init l) r
                                | otherwise -> go (go init r) l
                    _          -> go init emm
        where
          go z' Nil           = z'
          go z' (Tip _ x)     = f x z'
          go z' (Bin _ _ l r) = go (go z' r) l
    foldrWithKey f init (KEC emm) = foldrWithKey_ (\k -> f $! K k) init emm
    keysSet (KEC emm) = EMS.KSC $ go emm
        where
          go Nil        = EMS.Nil
          go (Tip kx _) = EMS.Tip (EMS.prefixOf kx) (EMS.bitmapOf kx)
          go (Bin p m l r)
              | m .&. EMS.suffixBitMask == 0 = EMS.Bin p m (go l) (go r)
              | otherwise = EMS.Tip (p .&. EMS.prefixBitMask)
                            (computeBm (computeBm 0 l) r)
              where
                computeBm !acc (Bin _ _ l' r') = computeBm (computeBm acc l') r'
                computeBm !acc (Tip kx _)      = acc .|. EMS.bitmapOf kx
                computeBm !acc Nil             = acc
    fromSet f (EMS.KSC emm) = KEC $ fromSet_ (f . K . toEnum) emm
    findMin (KEC emm) =
        case emm of
          Nil             -> error "findMin: no minimal element"
          Tip k v         -> (K $ toEnum k, v)
          Bin _ m l r
              |   m < 0   -> go r
              | otherwise -> go l
        where go (Tip k v)      = (K $ toEnum k, v)
              go (Bin _ _ l' _) = go l'
              go Nil            = error "findMin: Nil"
    minViewWithKey (KEC emm) =
        goat emm >>= \(r, emm') -> return (r, KEC $ emm')
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
              go (Tip k y) = ((K $ toEnum k, y), Nil)
              go Nil = error "minViewWithKey Nil"
    union (KEC emm1) (KEC emm2) = KEC $ mergeWithKey' Bin const id id emm1 emm2
    unionWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' Bin go id id emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $! f (K $ toEnum k1) x1 x2

    difference (KEC emm1) (KEC emm2) =
        KEC $ go emm1 emm2
            where go = mergeWithKey' bin (\_ _ -> Nil) id (const Nil)
    {-# INLINE difference #-}
    differenceWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin combine id (const Nil) emm1 emm2
            where
              combine = \(Tip k1 x1) (Tip _ x2)
                      -> case f (K $ toEnum k1) x1 x2 of
                           Nothing -> Nil
                           Just x -> x `seq` Tip k1 x

    intersection (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin const (const Nil) (const Nil) emm1 emm2
    intersectionWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin go (const Nil) (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $! f (K $ toEnum k1) x1 x2

    equal (KEC emm1) (KEC emm2) = emm1 == emm2
    nequal (KEC emm1) (KEC emm2) = emm1 /= emm2

{---------------------------------------------------------------------
 Instances
---------------------------------------------------------------------}

instance (Show v) => Show (EnumMapMap (K k) v) where
    show (KEC emm) = show emm

instance NFData v => NFData (EnumMapMap (K k) v) where
    rnf (KEC emm) = go emm
        where
          go Nil           = ()
          go (Tip _ v)     = rnf v
          go (Bin _ _ l r) = go l `seq` go r

instance (NFData k) => NFData (K k)
    where
      rnf (K k) = rnf k

instance HasSKey (K k) where
    type Skey (K k) = EMS.S k
    toS (K !k) = EMS.S k
    toK (EMS.S !k) = K k

{---------------------------------------------------------------------
 Split/Join Keys
---------------------------------------------------------------------}

type instance Plus (K k1) k2 = k1 :& k2

instance IsSplit (k :& t) Z where
    type Head (k :& t) Z = K k
    type Tail (k :& t) Z = t
    splitKey Z (KCC emm) = KEC $ emm

instance (Enum k1, k1 ~ k2) => SubKey (K k1) (k2 :& t2) v where
    type Result (K k1) (k2 :& t2) v = EnumMapMap t2 v
    member (K key) (KCC emm) = member_ (fromEnum key) emm
    singleton (K key) = KCC . Tip (fromEnum key)
    lookup (K key') (KCC emm) = lookup_ (fromEnum key') emm
    insert (K key') val (KCC emm) = KCC $ insert_ (fromEnum key') val emm
    insertWithKey f !k@(K key') val (KCC emm) =
        KCC $ insertWK (f k) (fromEnum key') val emm
    delete !(K key') (KCC emm) = KCC $ delete_ (fromEnum key') emm
instance (Enum k) => SubKey (K k) (K k) v where
    type Result (K k) (K k) v = v
    member (K key) (KEC emm) = member_ (fromEnum key) emm
    singleton !(K key) !val = KEC $! Tip (fromEnum key) val
    lookup (K key') (KEC emm) = lookup_ (fromEnum key') emm
    insert !(K key') !val (KEC emm) = KEC $ insert_ (fromEnum key') val emm
    insertWithKey f !k@(K key') !val (KEC emm) =
        KEC $ insertWK (f k) (fromEnum key') val emm
    delete !(K key') (KEC emm) = KEC $ delete_ (fromEnum key') emm

instance (Enum k1, k1 ~ k2) => SubKeyS (k1 :& t) (EMS.S k2) where
    intersectSet (KCC emm) (EMS.KSC ems) = KCC $ intersectSet_ emm ems
    differenceSet (KCC emm) (EMS.KSC ems) = KCC $ differenceSet_ emm ems

instance (Enum k) => SubKeyS (K k) (EMS.S k) where
    intersectSet (KEC emm) (EMS.KSC ems) = KEC $ intersectSet_ emm ems
    differenceSet (KEC emm) (EMS.KSC ems) = KEC $ differenceSet_ emm ems

member_ :: Key -> EMM k v -> Bool
member_ key emm = go emm
    where
      go t = case t of
               Bin _ m l r -> case zero key m of
                                True  -> go l
                                False -> go r
               Tip kx _    -> key == kx
               Nil         -> False

lookup_ :: Key -> EMM k v -> Maybe v
lookup_ !key emm = go emm
    where
      go t = case t of
               Bin _ m l r
                   | zero key m -> go l
                   | otherwise  -> go r
               Tip kx x         -> if kx == key then Just x else Nothing
               Nil              -> Nothing

insert_ :: Key -> v -> EMM k v -> EMM k v
insert_ !key !val emm =
    case emm of
      Bin p m l r
          | nomatch key p m -> join key (Tip key val) p emm
          | zero key m      -> Bin p m (insert_ key val l) r
          | otherwise       -> Bin p m l (insert_ key val r)
      Tip ky _
          | key == ky       -> Tip key val
          | otherwise       -> join key (Tip key val) ky emm
      Nil                   -> Tip key val

insertWK :: (v -> v -> v) -> Key -> v -> EMM k v -> EMM k v
insertWK f !key val = go
    where
      go emm =
          case emm of
            Bin p m l r
                | nomatch key p m -> join key (Tip key val) p emm
                | zero key m      -> Bin p m (go l) r
                | otherwise       -> Bin p m l (go r)
            Tip ky y
                | key == ky       -> Tip key (f val y)
                | otherwise       -> join key (Tip key val) ky emm
            Nil                   -> Tip key val

delete_ :: Key -> EMM k v -> EMM k v
delete_ !key emm = go emm
    where go t = case t of
                   Bin p m l r | nomatch key p m -> t
                               | zero key m      -> bin p m (go l) r
                               | otherwise       -> bin p m l (go r)
                   Tip ky _    | key == ky       -> Nil
                               | otherwise       -> t
                   Nil                           -> Nil

fromSet_ :: (Key -> v) -> EMS.EMS k -> EMM k v
fromSet_ f = go
    where
      go EMS.Nil           = Nil
      go (EMS.Bin p m l r) = Bin p m (go l) (go r)
      go (EMS.Tip key bm)  = buildTree f key bm (EMS.suffixBitMask + 1)
      buildTree g !prefix !bmask bits =
          case bits of
            0 -> Tip prefix $! (f prefix)
            _ -> case intFromNat ((natFromInt bits) `shiftRL` 1) of
                   bits2 | bmask .&. ((1 `shiftLL` bits2) -1) == 0 ->
                             buildTree g (prefix + bits2)
                                           (bmask `shiftRL` bits2) bits2
                         | (bmask `shiftRL` bits2) .&.
                           ((1 `shiftLL` bits2) - 1) == 0 ->
                               buildTree g prefix bmask bits2
                         | otherwise ->
                             Bin prefix bits2
                                     (buildTree g prefix bmask bits2)
                                     (buildTree g (prefix + bits2)
                                      (bmask `shiftRL` bits2)
                                      bits2)

{-# INLINE fromSet_ #-}

intersectSet_ :: EMM k v -> EMS.EMS k -> EMM k v
intersectSet_ emm ems =
    mergeWithKey' bin const (const Nil) (const Nil) emm ems'
        where ems' = fromSet_ (\_ -> ()) ems

differenceSet_ :: EMM k v -> EMS.EMS k -> EMM k v
differenceSet_ emm ems =
    mergeWithKey' bin (\_ _ -> Nil) id (const Nil) emm ems'
        where ems' = fromSet_ (\_ -> ()) ems
