{-# LANGUAGE StrictData #-}
{-|
Module      : Data.LfudaCache
Description : Pure LFUDA, GDSF, and LFU cache implementations
Copyright   : (c) 2026 philippedev101
License     : Apache-2.0
Maintainer  : philippedev101@gmail.com
Stability   : experimental

Pure, immutable cache with three eviction policies: __LFUDA__, __GDSF__, and __LFU__.

== Eviction policies

* __LFUDA__ (Least Frequently Used with Dynamic Aging) — combines access
  frequency with a global /age/ counter that advances on every eviction.
  New entries start with @frequency + age@ as their priority, which prevents
  long-lived but rarely accessed items from permanently occupying the cache
  (a common weakness of plain LFU).

* __GDSF__ (Greedy Dual-Size Frequency) — extends LFUDA by factoring in
  entry size: @frequency + age × size@. Useful when cached values have
  varying costs.

* __LFU__ (Least Frequently Used) — evicts the entry with the lowest access
  frequency. No aging is applied, so frequently accessed items are never
  evicted regardless of how long ago they were last accessed.

== Quick start

@
import Data.LfudaCache
import Prelude hiding ('lookup')

example :: (Maybe String, LfudaCache String String)
example =
  let cache  = 'newLFUDA' 100
      cache' = 'insert' \"hello\" \"world\" cache
  in  case 'lookup' \"hello\" cache' of
        Just (val, cache'') -> (Just val, cache'')
        Nothing             -> (Nothing, cache')
@

== Complexity

All operations are /O(log n)/ in the number of cached entries, backed by a
hash-priority search queue ('Data.HashPSQ.HashPSQ').

== Lookup vs Peek

'lookup' increments the entry's access frequency (affecting future eviction
priority). 'peek' returns the value without any side effect on frequency —
useful for monitoring or read-only inspection.
-}
module Data.LfudaCache
  ( -- * Cache type
    LfudaCache
  , CachePolicy(..)
  , Age

  -- * Construction
  , newLFUDA
  , newGDSF
  , newLFU
  , newCache

  -- * Insertion
  , insert
  , insertView

  -- * Lookup
  , lookup
  , peek
  , contains

  -- * Deletion
  , remove
  , purge

  -- * Size and metadata
  , keys
  , size
  , age
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import GHC.Generics (Generic)
import GHC.Types (Type)
import Prelude hiding (lookup)
import qualified Data.HashPSQ as HashPSQ


-- | Eviction policy. See the module documentation for a description of each.
type CachePolicy :: Type
data CachePolicy
  = LFUDA  -- ^ Least Frequently Used with Dynamic Aging.
           -- Priority = @frequency + age@.
  | GDSF   -- ^ Greedy Dual-Size Frequency.
           -- Priority = @frequency + age × size@.
  | LFU    -- ^ Plain Least Frequently Used (no aging).
           -- Priority = @frequency@.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Access count for a cache entry. Incremented on every 'lookup' hit.
type Frequency :: Type
type Frequency = Int64

-- | Cache age counter. Under LFUDA\/GDSF this advances on every eviction,
-- ensuring that newly inserted entries are not immediately evicted just
-- because older entries accumulated high frequency counts.
type Age :: Type
type Age = Int64

-- | Internal priority value computed from 'Frequency', 'Size', and 'Age'
-- according to the active 'CachePolicy'. The entry with the /lowest/
-- priority is evicted first.
type Priority :: Type
type Priority = Int64

-- | Logical size of a cache entry (used by the 'GDSF' policy).
-- Currently fixed at @1@ for every entry.
type Size :: Type
type Size = Int64

-- | Internal result of an eviction attempt (not exported).
type EvictionResult :: Type -> Type -> Type
type role EvictionResult representational representational
data EvictionResult k v =
  EvictionResult
  { evicted :: !Bool          -- ^ Whether an eviction occurred
  , evictedKey :: !(Maybe k)  -- ^ The key that was evicted (if any)
  , evictedValue :: !(Maybe v)  -- ^ The value that was evicted (if any)
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | An immutable, bounded cache parameterised by key type @k@ and value
-- type @v@. The eviction strategy is determined by the 'CachePolicy' chosen
-- at construction time.
--
-- The cache supports 'Functor', 'Foldable', and 'Traversable' over values,
-- as well as 'Eq', 'Show', and 'NFData'.
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
  {-# INLINABLE fmap #-}
  fmap f (LfudaCache cap sizeVal ageVal policy queue) =
    LfudaCache cap sizeVal ageVal policy (mapQueue f queue)
    where
      mapQueue :: forall v v'. (v -> v') -> HashPSQ.HashPSQ k Priority (Frequency, Size, v) -> HashPSQ.HashPSQ k Priority (Frequency, Size, v')
      mapQueue g = HashPSQ.fromList . map mapEntry . HashPSQ.toList
        where
          mapEntry :: (k, Priority, (Frequency, Size, v)) -> (k, Priority, (Frequency, Size, v'))
          mapEntry (k, p, (freq, s, v)) = (k, p, (freq, s, g v))

-- Manual implementation of Foldable
instance (Hashable k, Ord k) => Foldable (LfudaCache k) where
  {-# INLINABLE foldr #-}
  foldr f z = foldr (\(_, _, (_, _, v)) acc -> f v acc) z . HashPSQ.toList . lfudaQueue

-- Manual implementation of Traversable
instance (Hashable k, Ord k) => Traversable (LfudaCache k) where
  {-# INLINABLE traverse #-}
  traverse f (LfudaCache cap sizeVal ageVal policy queue) =
    LfudaCache cap sizeVal ageVal policy <$> traverseQueue f queue
    where
      traverseQueue :: forall f v v'. Applicative f =>
                     (v -> f v') ->
                     HashPSQ.HashPSQ k Priority (Frequency, Size, v) ->
                     f (HashPSQ.HashPSQ k Priority (Frequency, Size, v'))
      traverseQueue g = fmap HashPSQ.fromList . traverse transformEntry . HashPSQ.toList
        where
          transformEntry :: (k, Priority, (Frequency, Size, v)) -> f (k, Priority, (Frequency, Size, v'))
          transformEntry (k, p, (freq, s, v)) = (\v' -> (k, p, (freq, s, v'))) <$> g v

-- | Create a new empty cache with the given maximum capacity and eviction
-- policy. Calls 'error' if @capacity < 1@.
--
-- >>> size (newCache 100 LFUDA)
-- 0
{-# INLINABLE newCache #-}
newCache :: Int -> CachePolicy -> LfudaCache k v
newCache capacity policy
  | capacity < 1 = error "LfudaCache.new: capacity < 1"
  | otherwise    = LfudaCache capacity 0 0 policy HashPSQ.empty

-- | @'newLFUDA' cap@ — shorthand for @'newCache' cap 'LFUDA'@.
{-# INLINABLE newLFUDA #-}
newLFUDA :: Int -> LfudaCache k v
newLFUDA capacity = newCache capacity LFUDA

-- | @'newGDSF' cap@ — shorthand for @'newCache' cap 'GDSF'@.
{-# INLINABLE newGDSF #-}
newGDSF :: Int -> LfudaCache k v
newGDSF capacity = newCache capacity GDSF

-- | @'newLFU' cap@ — shorthand for @'newCache' cap 'LFU'@.
{-# INLINABLE newLFU #-}
newLFU :: Int -> LfudaCache k v
newLFU capacity = newCache capacity LFU

-- | Compute the eviction priority for an entry under the given policy.
-- Lower priority ⇒ evicted first.
{-# INLINE calculatePriority #-}
calculatePriority :: CachePolicy -> Frequency -> Size -> Age -> Priority
calculatePriority LFUDA freq _ ageValue = freq + ageValue
calculatePriority GDSF  freq sizeValue ageValue = freq + ageValue * sizeValue
calculatePriority LFU   freq _ _ = freq

-- | Common logic for insert operations
{-# INLINABLE prepareInsert #-}
prepareInsert :: (Hashable k, Ord k) => k -> v -> LfudaCache k v
              -> (Bool, LfudaCache k v)
prepareInsert key val c =
  let initialFreq :: Frequency
      initialFreq = 1
      entrySize :: Size
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
{-# INLINABLE trim #-}
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

-- | Insert a key–value pair. If the key already exists its value is
-- replaced and its frequency is reset to @1@. When the cache is at
-- capacity the lowest-priority entry is evicted first.
--
-- Use 'insertView' if you need to know /which/ entry was evicted.
--
-- /O(log n)/
--
-- >>> let c = insert "a" 1 (newLFUDA 2)
-- >>> size c
-- 1
{-# INLINABLE insert #-}
insert :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> LfudaCache k v
insert key val c =
  let (_, c') = prepareInsert key val c
      (_, c'') = trim c'
  in c''

-- | Like 'insert', but also returns the evicted entry (if any) as
-- @'Just' (key, value)@, or 'Nothing' when no eviction was necessary.
--
-- /O(log n)/
--
-- >>> let (ev1, c)  = insertView "a" 1 (newLFUDA 1)
-- >>> ev1
-- Nothing
-- >>> let (ev2, _) = insertView "b" 2 c
-- >>> ev2
-- Just ("a",1)
{-# INLINABLE insertView #-}
insertView :: (Hashable k, Ord k) => k -> v -> LfudaCache k v -> (Maybe (k, v), LfudaCache k v)
insertView key val c =
  let (_, c') = prepareInsert key val c
      (evictionResult, c'') = trim c'
  in case (evictedKey evictionResult, evictedValue evictionResult) of
       (Just k, Just v) -> (Just (k, v), c'')
       _ -> (Nothing, c'')

-- | Look up a key, returning its value and an updated cache with the
-- entry's frequency incremented. Returns 'Nothing' on a cache miss.
--
-- Use 'peek' if you do not want the frequency bump.
--
-- /O(log n)/
--
-- >>> let c = insert "a" 1 (newLFUDA 2)
-- >>> fmap fst (lookup "a" c)
-- Just 1
-- >>> lookup "z" c
-- Nothing
{-# INLINABLE lookup #-}
lookup :: (Hashable k, Ord k) => k -> LfudaCache k v -> Maybe (v, LfudaCache k v)
lookup k c =
  case HashPSQ.lookup k (lfudaQueue c) of
    Nothing -> Nothing
    Just (_, (freq, entrySize, v)) ->
      let newFreq = freq + 1
          newPriority = calculatePriority (lfudaPolicy c) newFreq entrySize (lfudaAge c)
          c' = c { lfudaQueue = HashPSQ.insert k newPriority (newFreq, entrySize, v) (lfudaQueue c) }
      in Just (v, c')

-- | Test whether a key is present in the cache. Does /not/ affect
-- the entry's frequency.
--
-- /O(log n)/
--
-- >>> let c = insert "a" 1 (newLFUDA 2)
-- >>> contains "a" c
-- True
-- >>> contains "z" c
-- False
{-# INLINABLE contains #-}
contains :: (Hashable k, Ord k) => k -> LfudaCache k v -> Bool
contains k = HashPSQ.member k . lfudaQueue

-- | Retrieve a value without incrementing its access frequency.
-- Useful for read-only inspection, monitoring, or debugging.
--
-- /O(log n)/
--
-- >>> let c = insert "a" 1 (newLFUDA 2)
-- >>> peek "a" c
-- Just 1
-- >>> peek "z" c
-- Nothing
{-# INLINABLE peek #-}
peek :: (Hashable k, Ord k) => k -> LfudaCache k v -> Maybe v
peek k c = (\(_, (_, _, v)) -> v) <$> HashPSQ.lookup k (lfudaQueue c)

-- | Remove a key from the cache. Returns the cache unchanged if the key
-- is not present.
--
-- /O(log n)/
--
-- >>> let c = insert "a" 1 (newLFUDA 2)
-- >>> size (remove "a" c)
-- 0
-- >>> size (remove "z" c)
-- 1
{-# INLINABLE remove #-}
remove :: (Hashable k, Ord k) => k -> LfudaCache k v -> LfudaCache k v
remove k c =
  case HashPSQ.lookup k (lfudaQueue c) of
    Nothing -> c
    Just _  -> c { lfudaSize  = lfudaSize c - 1
                 , lfudaQueue = HashPSQ.delete k (lfudaQueue c)
                 }

-- | Remove all entries from the cache, resetting 'size' to @0@.
-- The capacity and policy are preserved.
--
-- /O(1)/
--
-- >>> size (purge (insert "a" 1 (newLFUDA 2)))
-- 0
{-# INLINABLE purge #-}
purge :: LfudaCache k v -> LfudaCache k v
purge c = c { lfudaSize  = 0
            , lfudaQueue = HashPSQ.empty
            }

-- | The current age of the cache. Under 'LFUDA' and 'GDSF' the age
-- advances each time an entry is evicted; under 'LFU' it stays at @0@.
--
-- /O(1)/
{-# INLINABLE age #-}
age :: LfudaCache k v -> Age
age = lfudaAge

-- | All keys currently in the cache, ordered from highest to lowest
-- eviction priority (i.e. the entry most likely to survive eviction
-- comes first).
--
-- /O(n log n)/
{-# INLINABLE keys #-}
keys :: (Hashable k, Ord k) => LfudaCache k v -> [k]
keys = map (\(k, _, _) -> k) . reverse . HashPSQ.toList . lfudaQueue

-- | The number of entries currently stored in the cache.
--
-- /O(1)/
{-# INLINABLE size #-}
size :: LfudaCache k v -> Int
size = lfudaSize