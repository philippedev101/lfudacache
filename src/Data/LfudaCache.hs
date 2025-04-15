{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StrictData #-}
{-|
Pure API to an LFUDA (Least Frequently Used with Dynamic Aging) cache.

The LFUDA algorithm is a variant of LFU that uses dynamic aging to prevent
cache pollution by infrequently accessed but long-lived entries. It works by
incrementing a cache age value when items are evicted and including this
age in the priority calculations for cache entries.

This module provides implementations for:
- LFUDA: Least Frequently Used with Dynamic Aging
- GDSF: Greedy Dual-Size Frequency
- LFU: Least Frequently Used (without dynamic aging)

Based on the Go implementation from github.com/bparli/lfuda-go
-}
module Data.LfudaCache
  ( -- * Types
    LfudaCache
  , CachePolicy(..)
  , Frequency
  , Age
  , Priority
  , EvictionResult(..)

  -- * Construction
  , empty
  , new
  , newGDSF
  , newLFU
  , newCache

  -- * Basic Operations
  , insert
  , insertView
  , lookup
  , set
  , remove
  , purge

  -- * Query Operations
  , contains
  , peek
  , peekOrInsert
  , containsOrSet
  , peekOrSet

  -- * Cache Information
  , keys
  , size
  , age
  ) where

import qualified Data.HashPSQ as HashPSQ
import Data.Hashable (Hashable)
import Data.Maybe (isNothing, isJust)
import Prelude hiding (lookup)
import Control.DeepSeq (NFData)
import Data.Int
import GHC.Generics (Generic)
import GHC.Types (Type)

-- | Cache policy selection for different eviction strategies
type CachePolicy :: Type
data CachePolicy = LFUDA  -- ^ Least Frequently Used with Dynamic Aging
                 | GDSF   -- ^ Greedy Dual-Size Frequency
                 | LFU    -- ^ Least Frequently Used (no dynamic aging)
                 deriving stock (Eq, Show, Generic)
                 deriving anyclass (NFData)

-- | Frequency of an element in the cache
type Frequency :: Type
type Frequency = Int64

-- | Age of the cache (used for dynamic aging)
type Age :: Type
type Age = Int64

-- | Priority in the cache is calculated differently based on the policy
type Priority :: Type
type Priority = Int64

-- | Size of cache entries (used for GDSF)
type Size :: Type
type Size = Int64

-- | Result of an eviction operation
type EvictionResult :: Type -> Type -> Type
type role EvictionResult representational representational
data EvictionResult k v =
  EvictionResult
  { evicted :: !Bool          -- ^ Whether an eviction occurred
  , evictedKey :: !(Maybe k)  -- ^ The key that was evicted (if any)
  , evictedValue :: !(Maybe v)  -- ^ The value that was evicted (if any)
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | LFUDA cache based on hashing with various policies.
type LfudaCache :: Type -> Type -> Type
type role LfudaCache representational representational
data LfudaCache k v = LfudaCache
  { lfudaCapacity :: {-# UNPACK #-} !Int       -- ^ The maximum number of elements in the cache
  , lfudaSize     :: {-# UNPACK #-} !Int       -- ^ The current number of elements in the cache
  , lfudaAge      :: {-# UNPACK #-} !Age       -- ^ The current age of the cache
  , lfudaPolicy   :: !CachePolicy              -- ^ The cache eviction policy to use
  , lfudaQueue    :: !(HashPSQ.HashPSQ k Priority (Frequency, Size, v)) -- ^ Underlying priority queue
  } deriving stock (Generic)

instance (Show k, Show v) => Show (LfudaCache k v) where
  show c = "LfudaCache { capacity=" ++ show (lfudaCapacity c) ++
           ", size=" ++ show (lfudaSize c) ++
           ", age=" ++ show (lfudaAge c) ++
           ", policy=" ++ show (lfudaPolicy c) ++
           ", entries=" ++ show (lfudaSize c) ++ " }"

instance (Eq k, Eq v, Hashable k, Ord k) => Eq (LfudaCache k v) where
  c1 == c2 = lfudaCapacity c1 == lfudaCapacity c2 &&
             lfudaSize c1 == lfudaSize c2 &&
             lfudaAge c1 == lfudaAge c2 &&
             lfudaPolicy c1 == lfudaPolicy c2 &&
             lfudaQueue c1 == lfudaQueue c2

instance (NFData k, NFData v) => NFData (LfudaCache k v)

-- Manual implementation of Functor
instance (Hashable k, Ord k) => Functor (LfudaCache k) where
  fmap f (LfudaCache cap size age policy queue) =
    LfudaCache cap size age policy (mapQueue f queue)
    where
      mapQueue g = HashPSQ.fromList . map mapEntry . HashPSQ.toList
        where
          mapEntry (k, p, (freq, s, v)) = (k, p, (freq, s, g v))

-- Manual implementation of Foldable
instance (Hashable k, Ord k) => Foldable (LfudaCache k) where
  foldr f z = foldr (\(_, _, (_, _, v)) acc -> f v acc) z . HashPSQ.toList . lfudaQueue

-- Manual implementation of Traversable
instance (Hashable k, Ord k) => Traversable (LfudaCache k) where
  traverse f (LfudaCache cap size age policy queue) =
    LfudaCache cap size age policy <$> traverseQueue f queue
    where
      traverseQueue g = fmap HashPSQ.fromList . traverse transformEntry . HashPSQ.toList
        where
          transformEntry (k, p, (freq, s, v)) = (\v' -> (k, p, (freq, s, v'))) <$> g v

-- | Create a new cache with the specified policy.
--
-- Returns an error if capacity is less than 1.
newCache :: Int -> CachePolicy -> LfudaCache k v
newCache capacity policy
  | capacity < 1 = error "LfudaCache.new: capacity < 1"
  | otherwise    = LfudaCache capacity 0 0 policy HashPSQ.empty

-- | Create a cache with LFUDA policy.
--
-- >>> let cache = empty 10
-- >>> size cache
-- 0
empty :: Int -> LfudaCache k v
empty capacity = newCache capacity LFUDA

-- | Alias for 'empty' for compatibility with the Go API.
new :: Int -> LfudaCache k v
new = empty

-- | Create a new cache with GDSF policy.
--
-- >>> let cache = newGDSF 10
-- >>> lfudaPolicy cache
-- GDSF
newGDSF :: Int -> LfudaCache k v
newGDSF capacity = newCache capacity GDSF

-- | Create a new cache with LFU policy.
--
-- >>> let cache = newLFU 10
-- >>> lfudaPolicy cache
-- LFU
newLFU :: Int -> LfudaCache k v
newLFU capacity = newCache capacity LFU

-- | Calculate priority for an entry based on policy
{-# INLINE calculatePriority #-}
calculatePriority :: CachePolicy -> Frequency -> Size -> Age -> Priority
calculatePriority LFUDA freq _ age = freq + age
calculatePriority GDSF  freq size age = freq + age * size
calculatePriority LFU   freq _ _ = freq

-- | Common logic for insert operations
prepareInsert :: (Hashable k, Ord k) => k -> v -> LfudaCache k v
              -> (Bool, LfudaCache k v)
prepareInsert key val c =
  let initialFreq = 1
      entrySize = 1  -- Default size (could be parameterized in future)
      priority = calculatePriority (lfudaPolicy c) initialFreq entrySize (lfudaAge c)
      (mbOldVal, queue') = HashPSQ.insertView key priority (initialFreq, entrySize, val) (lfudaQueue c)
      sizeIncrease = isNothing mbOldVal
  in (sizeIncrease, c { lfudaSize  = if sizeIncrease
                                     then lfudaSize c + 1
                                     else lfudaSize c
                       , lfudaQueue = queue'
                       })

-- | Restore 'LfudaCache' invariants by evicting elements if cache exceeds capacity
-- Returns (eviction result, updated cache)
trim :: (Hashable k, Ord k) => LfudaCache k v -> (EvictionResult k v, LfudaCache k v)
trim c
  | lfudaSize c <= lfudaCapacity c =
      (EvictionResult False Nothing Nothing, c)
  | otherwise =
      case HashPSQ.findMin (lfudaQueue c) of
        Nothing ->
          (EvictionResult False Nothing Nothing, c)  -- Should not happen in practice
        Just (k, _, (freq, _, v)) ->
          let newAge = case lfudaPolicy c of
                LFU -> lfudaAge c     -- No aging for LFU
                _   -> freq           -- Set age to frequency
              c' = c { lfudaSize  = lfudaSize c - 1
                     , lfudaAge   = newAge
                     , lfudaQueue = HashPSQ.deleteMin (lfudaQueue c)
                     }
          in (EvictionResult True (Just k) (Just v), c')

-- | Insert an element into the 'LfudaCache'.
-- Returns a tuple with a boolean indicating if an eviction occurred,
-- and the updated cache.
--
-- >>> let c = empty 1
-- >>> let (evicted, c') = insert "key1" "value1" c
-- >>> evicted
-- False
-- >>> let (evicted, c'') = insert "key2" "value2" c'
-- >>> evicted
-- True
insert :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (Bool, LfudaCache k v)
insert key val c =
  let (_, c') = prepareInsert key val c
      (evictionResult, c'') = trim c'
  in (evicted evictionResult, c'')

-- | Set an element in the 'LfudaCache'.
-- Alias for 'insert' for compatibility with the Go API.
-- Returns True if an eviction occurred.
set :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (Bool, LfudaCache k v)
set = insert

-- | Insert an element into the 'LfudaCache' returning the evicted
-- element if any.
--
-- >>> let c = empty 1
-- >>> let (evicted1, c') = insertView "key1" "value1" c
-- >>> evicted1
-- Nothing
-- >>> let (evicted2, c'') = insertView "key2" "value2" c'
-- >>> evicted2
-- Just ("key1","value1")
insertView :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (Maybe (k, v), LfudaCache k v)
insertView key val c =
  let (_, c') = prepareInsert key val c
      (evictionResult, c'') = trim c'
  in case (evictedKey evictionResult, evictedValue evictionResult) of
       (Just k, Just v) -> (Just (k, v), c'')
       _ -> (Nothing, c'')

-- | Get an element from the cache and update its frequency.
--
-- >>> let c = empty 2
-- >>> let (_, c') = insert "key1" "value1" c
-- >>> lookup "key1" c'
-- Just ("value1",LfudaCache { capacity=2, size=1, age=0, policy=LFUDA, entries=1 })
-- >>> lookup "key2" c'
-- Nothing
lookup :: (Hashable k, Ord k) => k -> LfudaCache k v -> Maybe (v, LfudaCache k v)
lookup k c =
  case HashPSQ.lookup k (lfudaQueue c) of
    Nothing -> Nothing
    Just (_, (freq, entrySize, v)) ->
      let newFreq = freq + 1
          newPriority = calculatePriority (lfudaPolicy c) newFreq entrySize (lfudaAge c)
          c' = c { lfudaQueue = HashPSQ.insert k newPriority (newFreq, entrySize, v) (lfudaQueue c) }
      in Just (v, c')

-- | Check if a key exists in the cache without updating its frequency.
--
-- >>> let c = empty 2
-- >>> let (_, c') = insert "key1" "value1" c
-- >>> contains "key1" c'
-- True
-- >>> contains "key2" c'
-- False
contains :: (Hashable k, Ord k) => k -> LfudaCache k v -> Bool
contains k = HashPSQ.member k . lfudaQueue

-- | Peek at a value without updating its frequency.
--
-- >>> let c = empty 2
-- >>> let (_, c') = insert "key1" "value1" c
-- >>> peek "key1" c'
-- Just "value1"
-- >>> peek "key2" c'
-- Nothing
peek :: (Hashable k, Ord k) => k -> LfudaCache k v -> Maybe v
peek k c = (\(_, (_, _, v)) -> v) <$> HashPSQ.lookup k (lfudaQueue c)

-- | Check if a key is in the cache, and if not, insert it.
-- Returns (existed, evicted, cache)
--
-- >>> let c = empty 1
-- >>> let (exists1, evicted1, c') = containsOrInsert "key1" "value1" c
-- >>> (exists1, evicted1)
-- (False,False)
-- >>> let (exists2, evicted2, c'') = containsOrInsert "key1" "value2" c'
-- >>> (exists2, evicted2)
-- (True,False)
-- >>> let (exists3, evicted3, c''') = containsOrInsert "key2" "value2" c''
-- >>> (exists3, evicted3)
-- (False,True)
containsOrInsert :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (Bool, Bool, LfudaCache k v)
containsOrInsert k v c =
  if contains k c
    then (True, False, c)
    else let (wasEvicted, c') = insert k v c in (False, wasEvicted, c')

-- | Alias for 'containsOrInsert' with reordered return values for Go API compatibility.
containsOrSet :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (Bool, Bool, LfudaCache k v)
containsOrSet = containsOrInsert

-- | Peek at a value, and if not present, insert it.
-- Returns (value, existed, evicted, cache)
--
-- >>> let c = empty 1
-- >>> let (val1, exists1, evicted1, c') = peekOrInsert "key1" "value1" c
-- >>> (val1, exists1, evicted1)
-- ("value1",False,False)
-- >>> let (val2, exists2, evicted2, c'') = peekOrInsert "key1" "value2" c'
-- >>> (val2, exists2, evicted2)
-- ("value1",True,False)
peekOrInsert :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (v, Bool, Bool, LfudaCache k v)
peekOrInsert k v c =
  case peek k c of
    Just val -> (val, True, False, c)
    Nothing  -> let (wasEvicted, c') = insert k v c in (v, False, wasEvicted, c')

-- | Peek at a value, and if not present, insert it.
-- Alias for 'peekOrInsert' with a different return ordering for Go API compatibility.
peekOrSet :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (Maybe v, Bool, Bool, LfudaCache k v)
peekOrSet k v c =
  case peek k c of
    Just val -> (Just val, True, False, c)
    Nothing  -> let (wasEvicted, c') = insert k v c in (Nothing, False, wasEvicted, c')

-- | Remove an item from the cache.
-- Returns True if the item was present.
--
-- >>> let c = empty 2
-- >>> let (_, c') = insert "key1" "value1" c
-- >>> let (removed, c'') = remove "key1" c'
-- >>> removed
-- True
-- >>> let (removed', c''') = remove "key2" c''
-- >>> removed'
-- False
remove :: (Hashable k, Ord k) => k -> LfudaCache k v -> (Bool, LfudaCache k v)
remove k c =
  case HashPSQ.lookup k (lfudaQueue c) of
    Nothing -> (False, c)
    Just _  -> (True, c { lfudaSize  = lfudaSize c - 1
                        , lfudaQueue = HashPSQ.delete k (lfudaQueue c)
                        })

-- | Purge all items from the cache.
--
-- >>> let c = empty 2
-- >>> let (_, c') = insert "key1" "value1" c
-- >>> let (_, c'') = insert "key2" "value2" c'
-- >>> size c''
-- 2
-- >>> size (purge c'')
-- 0
purge :: LfudaCache k v -> LfudaCache k v
purge c = c { lfudaSize  = 0
            , lfudaQueue = HashPSQ.empty
            }

-- | Get the current age of the cache.
--
-- The age increases when items are evicted from the cache.
age :: LfudaCache k v -> Age
age = lfudaAge

-- | Get all keys in the cache, ordered by priority (highest to lowest).
--
-- >>> let c = empty 2
-- >>> let (_, c') = insert "key1" "value1" c
-- >>> let (_, c'') = insert "key2" "value2" c'
-- >>> keys c''
-- ["key1","key2"]
keys :: (Hashable k, Ord k) => LfudaCache k v -> [k]
keys = map (\(k, _, _) -> k) . reverse . HashPSQ.toList . lfudaQueue

-- | Get the current size of the cache.
--
-- >>> let c = empty 2
-- >>> let (_, c') = insert "key1" "value1" c
-- >>> size c'
-- 1
size :: LfudaCache k v -> Int
size = lfudaSize
