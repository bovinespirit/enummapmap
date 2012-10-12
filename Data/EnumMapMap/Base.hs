{-# LANGUAGE MagicHash, TypeFamilies, MultiParamTypeClasses,
    BangPatterns, FlexibleInstances, TypeOperators,
    FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMapMap.Base
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
-- Based on Data.IntMap.Base.
--
-- This defines the EnumMapMap (k :& t) v instance, and the Key data types.  The
-- terminating key type is K, and the EnumMapMap (K k) v instances are defined
-- in EnumMapMap.Lazy and EnumMapMap.Strict.
-----------------------------------------------------------------------------

module Data.EnumMapMap.Base(
            -- * Key types
            (:&)(..), K(..), N(..), Z(..),
            d1, d2, d3, d4, d5, d6, d7, d8, d9, d10,
            -- * Split/Join Keys
            IsSplit(..),
            Plus,
            -- * Internal
            -- ** IsEMM
            EMM(..),
            IsEmm(..),
            EnumMapMap(..),
            -- ** EMM internals
            mergeWithKey',
            mapWithKey_,
            foldrWithKey_,
            foldlStrict,
            -- ** IntMap internals
            Key,
            bin,
            tip,
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

import           Control.DeepSeq (NFData(rnf))
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

infixr 3 :&
-- | Multiple keys are joined by the (':&') constructor and terminated with 'K'.
--
-- > multiKey :: Int :& Int :& K Int
-- > multiKey = 5 :& 6 :& K 5
--
data k :& t = !k :& !t
                   deriving (Show, Eq)
-- | Keys are terminated with the 'K' type
--
-- > singleKey :: K Int
-- > singleKey = K 5
--
data K k = K !k
           deriving (Show, Eq)
data Z = Z
data N n = N n

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
d2 ::  N(Z)
d2  =  N d1
d3 ::  N(N(Z))
d3  =  N d2
d4 ::  N(N(N(Z)))
d4  =  N d3
d5 ::  N(N(N(N(Z))))
d5  =  N d4
d6 ::  N(N(N(N(N(Z)))))
d6  =  N d5
d7 ::  N(N(N(N(N(N(Z))))))
d7  =  N d6
d8 ::  N(N(N(N(N(N(N(Z)))))))
d8  =  N d7
d9 ::  N(N(N(N(N(N(N(N(Z))))))))
d9  =  N d8
d10 :: N(N(N(N(N(N(N(N(N(Z)))))))))
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
    type Head (k :& t) (N n) = k :& (Head t n)
    type Tail (k :& t) (N n) = Tail t n
    splitKey (N n) (KCC emm) = KCC $ mapWithKey_ (\_ -> splitKey n) emm

type family Plus k1 k2 :: *
type instance Plus (k1 :& t) k2 = k1 :& (Plus t k2)

class IsEmm k where
    -- | A map of keys to values.  The keys are 'Enum' types but are stored as 'Int's
    -- so any keys with the same 'Int' value are treated as the same.  The aim is to
    -- provide typesafe indexing.
    data EnumMapMap k :: * -> *

    -- | No subtrees should be empty.  Returns 'True' if one is.
    emptySubTrees  :: EnumMapMap k v -> Bool
    emptySubTrees_ :: EnumMapMap k v -> Bool

    removeEmpties :: EnumMapMap k v -> EnumMapMap k v

    -- | Join a key so that an 'EnumMapMap' of
    -- 'EnumMapMap's becomes an 'EnumMapMap'.
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
    joinKey :: (IsEmm (Plus k k2)) =>
               EnumMapMap k (EnumMapMap k2 v)
            -> EnumMapMap (Plus k k2) v
    joinKey = removeEmpties . unsafeJoinKey

    -- | Join a key so that an 'EnumMapMap' of
    -- 'EnumMapMap's becomes an 'EnumMapMap'.  The unsafe version does not check
    -- for empty subtrees, so it is faster.
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
    -- | Is the key present in the 'EnumMapMap'?
    member :: k -> EnumMapMap k v -> Bool
    -- | An 'EnumMapMap' with one element
    --
    -- > singleton (5 :& K 3) "a" == fromList [(5 :& K 3, "a")]
    singleton :: k -> v -> EnumMapMap k v
    -- | Lookup up the value at a key in the 'EnumMapMap'.
    --
    -- > emm = fromList [(3 :& K 1, "a")]
    -- > lookup (3 :& K 1) emm == Just "a"
    -- > lookup (2 :& K 1) emm == Nothing
    --
    lookup :: k -> EnumMapMap k v -> Maybe v
    -- | Insert a new Key\/Value pair into the 'EnumMapMap'.
    insert :: k -> v -> EnumMapMap k v -> EnumMapMap k v
    -- | Insert with a combining function.
    insertWith :: (v -> v -> v)
                  -> k -> v -> EnumMapMap k v -> EnumMapMap k v
    insertWith f = insertWithKey (\_ -> f)
    -- | Insert with a combining function.
    insertWithKey :: (k -> v -> v -> v)
                  -> k -> v -> EnumMapMap k v -> EnumMapMap k v
    -- | Remove a key and it's value from the 'EnumMapMap'.  If the key is not
    -- present the original 'EnumMapMap' is returned.
    delete :: k -> EnumMapMap k v -> EnumMapMap k v
    -- | The expression (@'alter' f k emm@) alters the value at @k@, or absence thereof.
    -- 'alter' can be used to insert, delete, or update a value in an 'EnumMapMap'.
    alter :: (Maybe v -> Maybe v) -> k -> EnumMapMap k v -> EnumMapMap k v
    -- | Map a function over all values in the 'EnumMapMap'.
    map :: (v -> t) -> EnumMapMap k v -> EnumMapMap k t
    map f = mapWithKey (\_ -> f)
    -- | Map a function over all key\/value pairs in the 'EnumMapMap'.
    mapWithKey :: (k -> v -> t) -> EnumMapMap k v -> EnumMapMap k t
    -- | Fold the keys and values in the map using the given right-associative
    -- binary operator.
    foldrWithKey :: (k -> v -> t -> t) -> t -> EnumMapMap k v -> t
    -- |  Convert the 'EnumMapMap' to a list of key\/value pairs.
    toList :: EnumMapMap k v -> [(k, v)]
    toList = foldrWithKey (\k x xs -> (k, x):xs) []
    -- | Create a 'EnumMapMap' from a list of key\/value pairs.
    fromList :: [(k, v)] -> EnumMapMap k v
    fromList = foldlStrict (\t (k, x) -> insert k x t) empty
    -- | The (left-biased) union of two 'EnumMapMap's.
    -- It prefers the first 'EnumMapMap' when duplicate keys are encountered.
    union :: EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    -- | The union of a list of maps.
    unions :: [EnumMapMap k v] -> EnumMapMap k v
    unions = foldlStrict union empty
    -- | The union with a combining function.
    unionWith :: (v -> v -> v)
              -> EnumMapMap k v -> EnumMapMap k v -> EnumMapMap k v
    unionWith f = unionWithKey (\_ -> f)
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
    differenceWith f = differenceWithKey (\_ -> f)
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
    intersectionWith f = intersectionWithKey (\_ -> f)
    -- | The intersection with a combining function.
    intersectionWithKey :: (k -> v1 -> v2 -> v3)
                        -> EnumMapMap k v1
                        -> EnumMapMap k v2
                        -> EnumMapMap k v3

    equal ::  Eq v => EnumMapMap k v -> EnumMapMap k v -> Bool
    nequal :: Eq v => EnumMapMap k v -> EnumMapMap k v -> Bool


instance (Enum k, IsEmm t) => IsEmm (k :& t) where
    data EnumMapMap (k :& t) v = KCC (EMM k (EnumMapMap t v))

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

    unsafeJoinKey (KCC emm) = KCC $ mapWithKey_ (\_ -> unsafeJoinKey) emm

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

    member !(key' :& nxt) (KCC emm) = go emm
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

    lookup (key :& nxt) (KCC emm) = go emm
        where
          go (Bin _ m l r)
              | zero (fromEnum key) m = go l
              | otherwise = go r
          go (Tip kx x)
             = case kx == (fromEnum key) of
                 True -> lookup nxt x
                 False -> Nothing
          go Nil = Nothing

    insert (key :& nxt) val (KCC emm)
        = KCC $ insertWith_ (insert nxt val) key (singleton nxt val) emm

    insertWithKey f k@(key :& nxt) val (KCC emm) =
        KCC $ insertWith_ go key (singleton nxt val) emm
            where
              go = insertWithKey (\_ -> f k) nxt val

    delete !(key :& nxt) (KCC emm) =
        KCC $ alter_ (delete nxt) (fromEnum key) emm

    alter f !(key :& nxt) (KCC emm) =
        KCC $ alter_ (alter f nxt) (fromEnum key) emm

    mapWithKey f (KCC emm) = KCC $ mapWithKey_ go emm
        where
          go k = mapWithKey (\nxt -> f $ k :& nxt)

    foldrWithKey f init (KCC emm) = foldrWithKey_ go init emm
        where
          go k val z = foldrWithKey (\nxt -> f $ k :& nxt) z val

    union (KCC emm1) (KCC emm2) = KCC $ mergeWithKey' binD go id id emm1 emm2
        where
          go = \(Tip k1 x1) (Tip _ x2) -> tip k1 $ union x1 x2
    unionWithKey f (KCC emm1) (KCC emm2) =
        KCC $ mergeWithKey' binD go id id emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ unionWithKey (g k1) x1 x2
              g k1 nxt = f $ (toEnum k1) :& nxt

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
                                              f $ (toEnum k1) :& nxt) x1 x2

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
                                                f $ (toEnum k1) :& nxt) x1 x2

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
alter_ :: (IsEmm b) =>
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

instance (Show v, Show (EnumMapMap t v)) => Show (EnumMapMap (k :& t) v) where
    show (KCC emm) = show emm

instance (NFData v, NFData (EnumMapMap t v)) => NFData (EnumMapMap (k :& t) v)
    where
      rnf (KCC emm) = go emm
          where
            go Nil           = ()
            go (Tip _ v)     = rnf v
            go (Bin _ _ l r) = go l `seq` go r

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

joinD :: (IsEmm b) =>
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

