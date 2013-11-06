{-# LANGUAGE
      BangPatterns,
      DeriveDataTypeable,
      FlexibleContexts,
      FlexibleInstances,
      GeneralizedNewtypeDeriving,
      MagicHash,
      MultiParamTypeClasses,
      StandaloneDeriving,
      TypeFamilies,
      TypeOperators,
      UndecidableInstances
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMapMap.Base
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
-- Based on Data.IntMap.Base.
--
-- This defines the @'EnumMapMap' (k ':&' t) v@ instance, and the Key data types.  The
-- terminating key type is K, and the @'EnumMapMap' (K k) v@ instances are defined
-- in EnumMapMap.Lazy and EnumMapMap.Strict.
-----------------------------------------------------------------------------

module Data.EnumMapMap.Base(
            -- * Key types
            (:&)(..), N(..), Z(..),
            d1, d2, d3, d4, d5, d6, d7, d8, d9, d10,
            -- * Split/Join Keys
            IsSplit(..),
            Plus,
            SubKey(..),
            -- * Internal
            -- ** IsEMM
            EMM(..),
            IsKey(..),
            EnumMapMap(..),
            -- ** SKey
            HasSKey(..),
            SubKeyS(..),
            -- ** EMM internals
            mergeWithKey',
            mapWithKey_,
            mapMaybeWithKey_,
            traverseWithKey_,
            foldrWithKey_,
            foldlStrict,
            -- ** IntMap internals
            Prefix,
            Mask,
            Nat,
            Key,
            intFromNat,
            natFromInt,
            shiftRL,
            shiftLL,
            branchMask,
            mask,
            bin,
            tip,
            shorter,
            nomatch,
            match,
            join,
            zero
) where

import           Prelude hiding (lookup,
                                 map,
                                 filter,
                                 foldr, foldl,
                                 null, init,
                                 head, tail)

import           Control.Applicative (Applicative(pure,(<*>)),(<$>))
import           Control.DeepSeq (NFData(rnf))
import           Data.Bits
import           Data.Default
import qualified Data.Foldable as FOLD
import           Data.Maybe (fromMaybe)
import           Data.SafeCopy
import           Data.Semigroup
import           Data.Traversable (Traversable(traverse))
import           Data.Typeable
import           GHC.Exts (Word(..), Int(..),
                           uncheckedShiftRL#, uncheckedShiftL#)

data EMM k v = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask
                     !(EMM k v) !(EMM k v)
             | Tip {-# UNPACK #-} !Int v
             | Nil
               deriving (Show)

type Nat    = Word
type Key    = Int
type Prefix = Int
type Mask   = Int

infixr 3 :&
-- | Multiple keys are joined by the (':&') constructor.
--
-- > multiKey :: Int :& Int :& K Int
-- > multiKey = 5 :& 6 :& K 5
--
data k :& t = !k :& !t
                   deriving (Show, Eq)

data Z = Z
data N n = N !n

-- | Split after 1 key.
--
-- > emm :: EnumMapMap (T1 :& T2 :& K T3) v
-- > splitKey d1 emm :: EnumMapMap (T1 :& K T2) (EnumMapMap (K T3) v)
d1 ::  Z
d1  =  Z
-- | Split after 2 keys.
--
-- > emm :: EnumMapMap (T1 :& T2 :& K T3) v
-- > splitKey d1 emm :: EnumMapMap (K T1) (EnumMapMap (T2 :& K T3) v)
d2 ::  N Z
d2  =  N d1
d3 ::  N(N Z)
d3  =  N d2
d4 ::  N(N(N Z))
d4  =  N d3
d5 ::  N(N(N(N Z)))
d5  =  N d4
d6 ::  N(N(N(N(N Z))))
d6  =  N d5
d7 ::  N(N(N(N(N(N Z)))))
d7  =  N d6
d8 ::  N(N(N(N(N(N(N Z))))))
d8  =  N d7
d9 ::  N(N(N(N(N(N(N(N Z)))))))
d9  =  N d8
d10 :: N(N(N(N(N(N(N(N(N Z))))))))
d10 =  N d9

class IsSplit k z where
    type Head k z :: *
    type Tail k z :: *
    -- | Split a key so that an 'EnumMapMap' becomes an 'EnumMapMap' of
    -- 'EnumMapMap's.
    --
    -- > newtype ID = ID Int deriving Enum
    -- > emm = empty :: EnumMapMap (Int :& K ID) Bool
    -- > res :: EnumMapMap (K ID) Bool
    -- > res = lookup (K 5) $ splitKey d1 emm
    --
    -- If the level is too high then the compilation will fail with an error
    --
    -- > emm = empty :: EnumMapMap (Int :& Int :& K Int) Bool -- 3 levels
    -- > res1 = splitKey d4 emm -- ERROR! Instance not found...
    -- > res2 = splitKey d3 emm -- ERROR! Instance not found...
    -- > res3 = splitKey d2 emm -- Good
    --
    splitKey :: z -> EnumMapMap k v
             -> EnumMapMap (Head k z) (EnumMapMap (Tail k z) v)

instance (IsSplit t n, Enum k) => IsSplit (k :& t) (N n) where
    type Head (k :& t) (N n) = k :& Head t n
    type Tail (k :& t) (N n) = Tail t n
    splitKey (N n) (KCC emm) = KCC $ mapWithKey_ (\_ -> splitKey n) emm

type family Plus k1 k2 :: *
type instance Plus (k1 :& t) k2 = k1 :& Plus t k2

class SubKey k1 k2 v where
    -- | k1 should be a prefix of k2.  If @k1 ~ k2@ then the 'Result' will be
    -- @v@.
    --
    -- > Result (K ID1) (ID1 :& K ID2) v        ~ EnumMapMap (K ID2) v
    -- > Result (ID1 :& K ID2) (ID1 :& K ID2) v ~ v
    -- > Result (ID1 :& K ID2) (K ID1) v        -- ERROR
    -- > Result (ID2 :& K ID1) (ID1 :& K ID2)   -- ERROR
    type Result k1 k2 v :: *

    -- | Is the key present in the 'EnumMapMap'?
    member :: k1 -> EnumMapMap k2 v -> Bool

    -- | An 'EnumMapMap' with one element
    --
    -- > singleton (5 :& K 3) "a" == fromList [(5 :& K 3, "a")]
    -- > singleton (K 5) $ singleton (K 2) "a" == fromList [(5 :& K 3, "a")]
    singleton :: k1 -> Result k1 k2 v -> EnumMapMap k2 v

    -- | Lookup up the value at a key in the 'EnumMapMap'.
    --
    -- > emm = fromList [(3 :& K 1, "a")]
    -- > lookup (3 :& K 1) emm == Just "a"
    -- > lookup (2 :& K 1) emm == Nothing
    --
    -- If the given key has less dimensions then the 'EnumMapMap' then a submap
    -- is returned.
    --
    -- > emm2 = fromList [(3 :& 2 :& K 1, "a"), (3 :& 2 :& K 4, "a")]
    -- > lookup (3 :& K 2) emm2 == Just $ fromList [(K 1, "a"), (K 4, "a")]
    --
    lookup :: (IsKey k1, IsKey k2) =>
              k1 -> EnumMapMap k2 v -> Maybe (Result k1 k2 v)

    -- | Insert a new key\/value pair into the 'EnumMapMap'.  Can also insert submaps.
    insert :: (IsKey k1, IsKey k2) =>
               k1 -> Result k1 k2 v -> EnumMapMap k2 v -> EnumMapMap k2 v

    -- | Insert with a combining function.  Can also insert submaps.
    insertWith :: (IsKey k1, IsKey k2) =>
                  (Result k1 k2 v -> Result k1 k2 v -> Result k1 k2 v)
               -> k1 -> Result k1 k2 v -> EnumMapMap k2 v -> EnumMapMap k2 v
    insertWith f = insertWithKey (const f)

    -- | Insert with a combining function.  Can also insert submaps.
    insertWithKey :: (IsKey k1, IsKey k2) =>
                     (k1 -> Result k1 k2 v -> Result k1 k2 v -> Result k1 k2 v)
                  -> k1 -> Result k1 k2 v -> EnumMapMap k2 v -> EnumMapMap k2 v

    -- | Remove a key and it's value from the 'EnumMapMap'.  If the key is not
    -- present the original 'EnumMapMap' is returned.
    delete :: (IsKey k1, IsKey k2) =>
              k1 -> EnumMapMap k2 v -> EnumMapMap k2 v

class SubKeyS k s where
    -- | The intersection of an 'EnumMapMap' and an 'EnumMapSet'.  If a key is
    -- present in the EnumMapSet then it will be present in the resulting
    -- 'EnumMapMap'.  Works with 'EnumMapSet's that are submaps of the
    -- 'EnumMapMap'.
    intersectSet :: (IsKey k, IsKey s) =>
                   EnumMapMap k v -> EnumMapMap s () -> EnumMapMap k v
    -- | The difference between an 'EnumMapMap' and an 'EnumMapSet'.  If a key
    -- is present in the 'EnumMapSet' it will not be present in the result.
    differenceSet :: (IsKey k, IsKey s) =>
                   EnumMapMap k v -> EnumMapMap s () -> EnumMapMap k v

class HasSKey k where
    type Skey k :: *
    -- | Convert a key terminated with 'K' into one terminated with 'S'.
    --
    -- > k = 1 :& 2 :& 'K' 3
    -- > toS k == 1 :& 2 :& 'S' 3
    --
    toS :: k -> Skey k
    -- | Convert a key terminated with 'S' into one terminated with 'K'.
    --
    -- > s = 1 :& 2 :& S 3
    -- > toK s == 1 :& 2 :& K 3
    toK :: Skey k -> k

instance (HasSKey t) => HasSKey (k :& t) where
    type Skey (k :& t) = k :& Skey t
    toS (k :& t) = (:&) k $! toS t
    toK (k :& t) = (:&) k $! toK t

class (Eq k) => IsKey k where
    -- | A map of keys to values.  The keys are 'Enum' types but are stored as 'Int's
    -- so any keys with the same 'Int' value are treated as the same.  The aim is to
    -- provide typesafe indexing.
    data EnumMapMap k :: * -> *

    -- | No subtrees should be empty.  Returns 'True' if one is.
    emptySubTrees  :: EnumMapMap k v -> Bool
    emptySubTrees_ :: EnumMapMap k v -> Bool

    -- | Remove empty subtrees.
    removeEmpties :: EnumMapMap k v -> EnumMapMap k v

    -- | Join a key so that an 'EnumMapMap' of 'EnumMapMap's becomes an
    -- 'EnumMapMap'.
    --
    -- > newtype ID = ID Int deriving Enum
    -- > emm :: EnumMapMap (K Int) (EnumMapMap (K ID) Bool)
    -- > res :: EnumMapMap (Int :& K ID) Bool
    -- > res = joinKey emm
    --
    -- 'joinKey' is the opposite of 'splitKey'.
    --
    -- > emm = empty :: EnumMapMap (Int :& Int :& K ID) Bool)
    -- > emm == joinKey $ splitKey d2 emm
    --
    joinKey :: (IsKey (Plus k k2)) =>
               EnumMapMap k (EnumMapMap k2 v)
            -> EnumMapMap (Plus k k2) v
    joinKey = removeEmpties . unsafeJoinKey

    -- | Join a key so that an 'EnumMapMap' of 'EnumMapMap's becomes an
    -- 'EnumMapMap'.  The unsafe version does not check for empty subtrees, so
    -- it is faster.
    --
    -- > newtype ID = ID Int deriving Enum
    -- > emm :: EnumMapMap (K Int) (EnumMapMap (K ID) Bool)
    -- > res :: EnumMapMap (Int :& K ID) Bool
    -- > res = unsafeJoinKey emm
    --
    unsafeJoinKey :: EnumMapMap k (EnumMapMap k2 v)
                  -> EnumMapMap (Plus k k2) v

    -- | The empty 'EnumMapMap'.
    empty :: EnumMapMap k v
    -- | Is the 'EnumMapMap' empty?
    --
    -- Submaps can never be empty, so the following should always hold true:
    --
    -- > emm :: EnumMapMap (Int :& Int :& K ID) Bool)
    -- > null $ splitKey x emm == False
    null :: EnumMapMap k v -> Bool
    -- | Number of elements in the 'EnumMapMap'.
    size :: EnumMapMap k v -> Int
    -- | The expression (@'alter' f k emm@) alters the value at @k@, or absence thereof.
    -- 'alter' can be used to insert, delete, or update a value in an 'EnumMapMap'.
    alter :: (Maybe v -> Maybe v) -> k -> EnumMapMap k v -> EnumMapMap k v
    -- | Map a function over all values in the 'EnumMapMap'.
    map :: (v -> t) -> EnumMapMap k v -> EnumMapMap k t
    map f = mapWithKey (const f)
    -- |  Map values and collect the 'Just' results.
    mapMaybe :: (v -> Maybe t) -> EnumMapMap k v -> EnumMapMap k t
    mapMaybe f = mapMaybeWithKey (\_ x -> f x)
    -- | Map keys\/values and collect the 'Just' results.
    mapMaybeWithKey :: (k -> v -> Maybe t) -> EnumMapMap k v -> EnumMapMap k t
    -- | Map a function over all key\/value pairs in the 'EnumMapMap'.
    mapWithKey :: (k -> v -> t) -> EnumMapMap k v -> EnumMapMap k t
    -- | @TraverseWithKey@ behaves exactly like a regular 'traverse' except that
    -- the traversing function also has access to the key associated with a
    -- value.
    traverseWithKey :: (Applicative t) =>
                       (k -> a -> t b) -> EnumMapMap k a -> t (EnumMapMap k b)
    -- | Fold the values in the 'EnumMapMap' using the given
    -- right-associative binary operator
    foldr :: (v -> t -> t) -> t -> EnumMapMap k v -> t
    -- | Fold the keys and values in the 'EnumMapMap' using the given right-associative
    -- binary operator.
    foldrWithKey :: (k -> v -> t -> t) -> t -> EnumMapMap k v -> t
    -- |  Convert the 'EnumMapMap' to a list of key\/value pairs.
    toList :: SubKey k k v =>
              EnumMapMap k v -> [(k, v)]
    toList = foldrWithKey (\k x xs -> (k, x):xs) []
    -- | Create a 'EnumMapMap' from a list of key\/value pairs.
    fromList :: (SubKey k k v, Result k k v ~ v) => [(k, v)] -> EnumMapMap k v
    fromList = foldlStrict (\t (k, x) -> insert k x t) empty
    -- | List of elements in ascending order of keys
    elems :: EnumMapMap k v -> [v]
    elems = foldr (:) []
    -- | List of keys
    keys :: EnumMapMap k v -> [k]
    keys = foldrWithKey (\k _ ks -> k:ks) []
    -- | The 'Data.EnumMapSet' of the keys. 'EnumMapMap' keys can be converted into
    -- 'Data.EnumMapSet' keys using 'toS', and back again using 'toK'.
    keysSet :: (HasSKey k) => EnumMapMap k v -> EnumMapMap (Skey k) ()
    -- | Build an 'EnumMapMap' from an 'EnumMapSet' and a function which for each
    -- key computes it's value
    fromSet :: HasSKey k => (k -> v) -> EnumMapMap (Skey k) () -> EnumMapMap k v
    -- | The minimal key and value of the 'EnumMapMap'.
    --
    -- > findMin empty -- ERROR, no minimal key
    -- > findMin $ fromList [(K 1, "a", K 3, "b")] == (K 1, a)
    findMin :: EnumMapMap k v -> (k, v)
    -- | Retrieves the minimal (key,value) pair of the EnumMapMap, and the
    -- EnumMapMap stripped of that element, or 'Nothing' if passed an empty map.
    minViewWithKey :: EnumMapMap k v -> Maybe ((k, v), EnumMapMap k v)
    deleteFindMin :: EnumMapMap k v -> ((k, v), EnumMapMap k v)
    deleteFindMin =
        fromMaybe(error "deleteFindMin: empty EnumMapMap has no minimal\
                        \ element") . minViewWithKey
    -- | The (left-biased) union of two 'EnumMapMap's.
    -- It prefers the first 'EnumMapMap' when duplicate keys are encountered.
    union :: EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    -- | The union of a list of maps.
    unions :: [EnumMapMap k v] -> EnumMapMap k v
    unions = foldlStrict union empty
    -- | The union of a list of maps with a combining function
    unionsWith :: (v -> v -> v) -> [EnumMapMap k v] -> EnumMapMap k v
    unionsWith f = foldlStrict (unionWith f) empty
    -- | The union with a combining function.
    unionWith :: (v -> v -> v)
              -> EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    unionWith f = unionWithKey (const f)
    -- | The union with a combining function.
    unionWithKey :: (k -> v -> v -> v)
                 -> EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    -- | Difference between two 'EnumMapMap's (based on keys).
    difference ::  EnumMapMap k v1 -> EnumMapMap k v2 -> EnumMapMap k v1
    -- | Difference with a combining function.
    differenceWith :: (v1 -> v2 -> Maybe v1)
                   -> EnumMapMap k v1
                   -> EnumMapMap k v2
                   -> EnumMapMap k v1
    differenceWith f = differenceWithKey (const f)
    -- | Difference with a combining function.
    differenceWithKey :: (k -> v1 -> v2 -> Maybe v1)
                      -> EnumMapMap k v1
                      -> EnumMapMap k v2
                      -> EnumMapMap k v1
    -- | The (left-biased) intersection of two 'EnumMapMap' (based on keys).
    intersection :: EnumMapMap k v1
                 -> EnumMapMap k v2
                 -> EnumMapMap k v1
    -- | The intersection with a combining function.
    intersectionWith :: (v1 -> v2 -> v3)
                     -> EnumMapMap k v1
                     -> EnumMapMap k v2
                     -> EnumMapMap k v3
    intersectionWith f = intersectionWithKey (const f)
    -- | The intersection with a combining function.
    intersectionWithKey :: (k -> v1 -> v2 -> v3)
                        -> EnumMapMap k v1
                        -> EnumMapMap k v2
                        -> EnumMapMap k v3

    equal ::  Eq v => EnumMapMap k v -> EnumMapMap k v -> Bool
    nequal :: Eq v => EnumMapMap k v -> EnumMapMap k v -> Bool

instance (Enum k, IsKey t1, IsKey t2, SubKey t1 t2 v) =>
    SubKey (k :& t1) (k :& t2) v where
    type Result (k :& t1) (k :& t2) v = Result t1 t2 v

    member !(key' :& nxt) (KCC emm) = key `seq` go emm
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

    singleton (key :& nxt) = KCC . Tip (fromEnum key) . singleton nxt

    lookup !(key' :& nxt) (KCC emm) = key `seq` go emm
        where
          go (Bin _ m l r)
             | zero key m = go l
             | otherwise = go r
          go (Tip kx x)
             = case kx == key of
                 True -> lookup nxt x
                 False -> Nothing
          go Nil = Nothing
          key = fromEnum key'

    insert (key :& nxt) val (KCC emm) =
        KCC $ insertWith_ (insert nxt val) key (singleton nxt val) emm

    insertWithKey f k@(key :& nxt) val (KCC emm) =
        KCC $ insertWith_ go key (singleton nxt val) emm
            where
              go = insertWithKey (\_ -> f k) nxt val

    delete !(key :& nxt) (KCC emm) =
        KCC $ alter_ (delete nxt) (fromEnum key) emm

instance (Enum k, IsKey t1, IsKey t2, SubKeyS t1 t2) =>
    SubKeyS (k :& t1) (k :& t2) where
        intersectSet (KCC emm) (KCC ems) =
            KCC $ mergeWithKey' binD go (const Nil) (const Nil) emm ems
                where
                  go = \(Tip k1 x1) (Tip _ x2) ->
                       tip k1 $ intersectSet x1 x2
        differenceSet (KCC emm) (KCC ems) =
            KCC $  mergeWithKey' binD go id (const Nil) emm ems
                where
                  go = \(Tip k1 x1) (Tip _ x2) ->
                       tip k1 $ differenceSet x1 x2

instance (Eq k, Enum k, IsKey t, HasSKey t) => IsKey (k :& t) where
    newtype EnumMapMap (k :& t) v = KCC (EMM k (EnumMapMap t v))

    emptySubTrees e@(KCC emm) =
        case emm of
          Nil -> False
          _   -> emptySubTrees_ e
    emptySubTrees_ (KCC emm) = go emm
        where
          go t = case t of
                   Bin _ _ l r -> go l || go r
                   Tip _ v     -> emptySubTrees_ v
                   Nil         -> True

    removeEmpties (KCC emm) = KCC $ go emm
        where
          go t = case t of
                   Bin p m l r -> bin p m (go l) (go r)
                   Tip k v     -> tip k (removeEmpties v)
                   Nil         -> Nil

    unsafeJoinKey (KCC emm) = KCC $ mapWithKey_ (const unsafeJoinKey) emm

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

    alter f !(key :& nxt) (KCC emm) =
        KCC $ alter_ (alter f nxt) (fromEnum key) emm

    mapWithKey f (KCC emm) = KCC $ mapWithKey_ go emm
        where
          go k = mapWithKey (\nxt -> f $! k :& nxt)

    mapMaybeWithKey f (KCC emm) = KCC $ mapMaybeWithKey_ go emm
        where
          go k = mapMaybeWithKey (\nxt -> f $! k :& nxt)

    traverseWithKey f (KCC emm) = KCC <$> traverseWithKey_ go emm
        where
          go k = traverseWithKey (\nxt -> f $! k :& nxt)

    foldr f init (KCC emm) = foldrWithKey_ (\_ val z -> foldr f z val) init emm

    foldrWithKey f init (KCC emm) = foldrWithKey_ go init emm
        where
          go k val z = foldrWithKey (\nxt -> f $! k :& nxt) z val

    keysSet (KCC emm) = KCC $ mapWithKey_ (const keysSet) emm

    fromSet f (KCC ems) = KCC $ mapWithKey_ go ems
        where
          go k = fromSet (\nxt -> f $! k :& nxt)

    findMin (KCC emm) =
        case emm of
          Nil             -> error "findMin: no minimal element"
          Tip k v         -> (toEnum k :& t, v')
              where (t, v') = findMin v
          Bin _ m l r
              |   m < 0   -> go r
              | otherwise -> go l
        where go (Tip k v)      = (toEnum k :& t, v')
                  where (t, v') = findMin v
              go (Bin _ _ l' _) = go l'
              go Nil            = error "findMin: Nil"

    minViewWithKey (KCC emm) =
        goat emm >>= \(r, emm') -> return (r, KCC emm')
            where
              goat t =
                  case t of
                    Nil                 -> Nothing
                    Bin p m l r | m < 0 ->
                                    case go r of
                                      (result, r') ->
                                          Just (result, binD p m l r')
                    _                   -> Just (go t)
              go (Bin p m l r) = case go l of
                                   (result, l') -> (result, binD p m l' r)
              go (Tip k y) = case minViewWithKey y of
                               Just ((t, v), y') ->
                                   ((toEnum k :& t, v), tip k y')
                               Nothing -> error "minViewWithKey: Nothing"
              go Nil = error "minViewWithKey Nil"

    union (KCC emm1) (KCC emm2) = KCC $ mergeWithKey' binD go id id emm1 emm2
        where
          go = \(Tip k1 x1) (Tip _ x2) -> tip k1 $ union x1 x2
    unionWithKey f (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go id id emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ unionWithKey (g k1) x1 x2
              g k1 nxt = f $! toEnum k1 :& nxt

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
                                              f $! toEnum k1 :& nxt) x1 x2

    intersection (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go (const Nil) (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   tip k1 $ intersection x1 x2
    intersectionWithKey f (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go (const Nil) (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   tip k1 $ intersectionWithKey (\nxt ->
                                                f $! toEnum k1 :& nxt) x1 x2

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

-- | 'alter_' is used to walk down the tree to find the 'EnumMapMap' to actually
-- change.  If the new 'EnumMapMap' is null then it's removed from the containing
-- 'EMM'.
alter_ :: (IsKey b) =>
          (EnumMapMap b v -> EnumMapMap b v)
       -> Key
       -> EMM a (EnumMapMap b v)
       -> EMM a (EnumMapMap b v)
alter_ f k = go
    where
      go t =
          case t of
            Bin p m l r | nomatch k p m -> joinD k (tip k $ f empty) p t
                        | zero k m      -> binD p m (go l) r
                        | otherwise     -> binD p m l (go r)
            Tip ky y    | k == ky       -> tip k $ f y
                        | otherwise     -> joinD k (tip k $ f empty) ky t
            Nil                         -> tip k $ f empty
{-# INLINE alter_ #-}

mapWithKey_ :: Enum k => (k -> v -> t) -> EMM k v -> EMM k t
mapWithKey_ f = go
    where
      go (Bin p m l r) = Bin p m (go l) (go r)
      go (Tip k x)     = Tip k (f (toEnum k) x)
      go Nil           = Nil
{-# INLINE mapWithKey_ #-}

mapMaybeWithKey_ :: (IsKey b, Enum key) =>
                    (key -> EnumMapMap b v -> EnumMapMap b t) ->
                    EMM a (EnumMapMap b v) ->
                    EMM a (EnumMapMap b t)
mapMaybeWithKey_ f = go
    where
      go (Bin p m l r) = binD p m (go l) (go r)
      go (Tip k x)     = tip k $ f (toEnum k) x
      go Nil           = Nil
{-# INLINE mapMaybeWithKey_ #-}

traverseWithKey_ :: (Enum k, Applicative t) =>
                    (k -> a -> t b) -> EMM k a -> t (EMM k b)
traverseWithKey_ f = go
    where
      go Nil = pure Nil
      go (Tip k v) = Tip k <$> f (toEnum k) v
      go (Bin p m l r) = Bin p m <$> go l <*> go r

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

-- | See 'IntMap' documentation for an explanation of 'mergeWithKey''.
mergeWithKey' :: (Prefix -> Mask -> EMM a v3 -> EMM a v3 -> EMM a v3)
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

    go t1'@(Bin {}) t2'@(Tip k2' _) = merge t2' k2' t1'
      where merge t2 k2 t1@(Bin p1 m1 l1 r1)
                | nomatch k2 p1 m1 = maybe_join p1 (g1 t1) k2 (g2 t2)
                | zero k2 m1 = bin' p1 m1 (merge t2 k2 l1) (g1 r1)
                | otherwise  = bin' p1 m1 (g1 l1) (merge t2 k2 r1)
            merge t2 k2 t1@(Tip k1 _)
                | k1 == k2  = f t1 t2
                | otherwise = maybe_join k1 (g1 t1) k2 (g2 t2)
            merge t2 _  Nil = g2 t2

    go t1@(Bin {}) Nil = g1 t1

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

instance (Eq v, IsKey k) => Eq (EnumMapMap k v) where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

instance Eq v => Eq (EMM k v) where
  t1 == t2  = equalE t1 t2
  t1 /= t2  = nequalE t1 t2

equalE :: Eq v => EMM k v -> EMM k v -> Bool
equalE (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && equalE l1 l2 && equalE r1 r2
equalE (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equalE Nil Nil = True
equalE _   _   = False

nequalE :: Eq v => EMM k v -> EMM k v -> Bool
nequalE (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || nequalE l1 l2 || nequalE r1 r2
nequalE (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequalE Nil Nil = False
nequalE _   _   = True

instance (IsKey k) => Functor (EnumMapMap k)
    where
      fmap = map

-- | This instance differs from the 'Monoid' instance in 'IntMap'.  Where the keys
-- are the same the values are combined using 'mappend'.
instance (IsKey k, Semigroup v) => Monoid (EnumMapMap k v) where
    mempty = empty
    mappend = unionWith (<>)
    mconcat = unionsWith (<>)

instance (IsKey k, Semigroup v) =>
    Semigroup (EnumMapMap k v) where
        (<>) = unionWith (<>)
        times1p _ a = a

instance (Show v, Show (EnumMapMap t v)) => Show (EnumMapMap (k :& t) v) where
    show (KCC emm) = show emm

instance (NFData v, NFData (EnumMapMap t v)) => NFData (EnumMapMap (k :& t) v)
    where
      rnf (KCC emm) = go emm
          where
            go Nil           = ()
            go (Tip _ v)     = rnf v
            go (Bin _ _ l r) = go l `seq` go r

instance (NFData k, NFData t) => NFData (k :& t)
    where
      rnf (k :& t) = rnf k `seq` rnf t

-- Foldable

instance (FOLD.Foldable (EnumMapMap t), Enum k, Eq k, IsKey t, HasSKey t) =>
    FOLD.Foldable (EnumMapMap (k :& t)) where
        fold (KCC emm) = go emm
            where
              go Nil           = mempty
              go (Tip _ v)     = FOLD.fold v
              go (Bin _ _ l r) = go l `mappend` go r
        foldr = foldr
        foldMap f (KCC emm) = go emm
            where
              go Nil           = mempty
              go (Tip _ v)     = FOLD.foldMap f v
              go (Bin _ _ l r) = go l `mappend` go r

instance (IsKey k, FOLD.Foldable (EnumMapMap k)) =>
    Traversable (EnumMapMap k) where
        traverse f = traverseWithKey (\_ -> f)

-- Default

instance (IsKey k) => Default (EnumMapMap k v) where
    def = empty

-- Typeable

deriving instance Typeable2 (:&)
deriving instance Typeable2 EnumMapMap

-- SafeCopy

instance (Enum a, SafeCopy b) => SafeCopy (a :& b) where
    getCopy = contain $ do
                a <- safeGet
                b <- safeGet
                return (toEnum a :& b)
    putCopy (a :& b) = contain $ do
                         safePut $ fromEnum a
                         safePut b
    errorTypeName _ = "(:&)"

{--------------------------------------------------------------------
  Nat conversion
--------------------------------------------------------------------}

natFromInt :: Int -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Int
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

shiftRL, shiftLL :: Nat -> Int -> Nat
shiftRL (W# x) (I# i) = W# (uncheckedShiftRL# x i)
shiftLL (W# x) (I# i) = W# (uncheckedShiftL#  x i)
{-# INLINE shiftRL #-}
{-# INLINE shiftLL #-}

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

joinD :: (IsKey b) =>
         Prefix -> EMM a (EnumMapMap b v)
      -> Prefix -> EMM a (EnumMapMap b v)
      -> EMM a (EnumMapMap b v)
joinD p1 t1 p2 t2
  | zero p1 m = binD p m t1 t2
  | otherwise = binD p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE joinD #-}

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
binD :: (IsKey b) =>
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

tip :: (IsKey b) => Key -> EnumMapMap b v -> EMM a (EnumMapMap b v)
tip k val
    | null val  = Nil
    | otherwise = Tip k val
{-# INLINE tip #-}

{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Key -> Mask -> Bool
zero i m
  = natFromInt i .&. natFromInt m == 0
{-# INLINE zero #-}

nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = mask i m /= p
{-# INLINE nomatch #-}

match i p m
  = mask i m == p
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
  = natFromInt m1 > natFromInt m2
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
          x6 -> (x6 `xor` shiftRL x6 1)
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

