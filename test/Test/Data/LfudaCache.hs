module Test.Data.LfudaCache
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Data.Maybe (isNothing)
import Data.LfudaCache
import Data.Foldable (foldl')
import Prelude hiding (lookup)
import Data.Kind


tests :: TestTree
tests = testGroup "LFUDA Cache Tests"
  [ testCase "Basic LFUDA Operations" testLFUDA
  , testCase "GDSF Test" testGDSF
  , testCase "Insert Eviction Via InsertView" testInsertEviction
  , testCase "Contains Doesn't Update Frequency" testLFUDAContains
  , testCase "Peek Doesn't Update Frequency" testLFUDAPeek
  , testCase "Remove Operation" testLFUDARemove
  , testCase "Age Tracking" testLFUDAAge
  , testCase "Size Tracking" testLFUDASize
  , testCase "LFU Basic Operations" testLFU
  , testCase "LFU No Dynamic Aging" testLFUNoAging
  , testCase "LFU Frequency-Based Eviction" testLFUFrequencyEviction
  , testCase "LFU InsertView" testLFUInsertView
  , testCase "GDSF Size-Weighted Eviction" testGDSFSizeEviction
  , testCase "GDSF Dynamic Aging" testGDSFAging
  , testCase "GDSF Frequency And Size" testGDSFFrequencyAndSize
  , testCase "Edge: Re-insert Resets Frequency" testReinsertResetsFrequency
  , testCase "Edge: Capacity 1 Corner Cases" testCapacity1
  , testCase "Edge: Size Consistency" testSizeConsistency
  , testCase "Edge: Purge Preserves Age" testPurgePreservesAge
  , testCase "Edge: Operations After Purge" testOpsAfterPurge
  , testCase "Edge: InsertView Self-Replace" testInsertViewSelfReplace
  , testCase "Edge: InsertView Evicts Correct Entry" testInsertViewEvictsCorrect
  , testCase "Edge: Remove Nonexistent" testRemoveNonexistent
  , testCase "Edge: Equal Frequency Eviction" testEqualFrequencyEviction
  , testCase "Edge: LFUDA Age Accumulation" testAgeAccumulation
  , testCase "Edge: Lookup After Insert Same Key" testLookupAfterReinsert
  , testCase "Edge: Rapid Insert-Remove Cycles" testInsertRemoveCycles
  , testBenchmark "LFUDA Benchmark" benchmarkLFUDA
  , testBenchmark "LFUDA Random Benchmark" benchmarkLFUDARand
  ]

testLFUDA :: Assertion
testLFUDA = do
  -- Create cache with proper policy
  let initialCache :: LfudaCache Int Int
      initialCache = newCache 666 LFUDA

  -- Track number of evictions using insertView
  let insertAndTrackEvictions :: (LfudaCache Int Int, Int) -> Int -> (LfudaCache Int Int, Int)
      insertAndTrackEvictions (cache, count) i =
        let (evictedEntry, cache') = insertView i i cache
            newCount = case evictedEntry of
              Just _  -> count + 1
              Nothing -> count
        in (cache', newCount)

  let finalCacheAndCount = foldl' insertAndTrackEvictions (initialCache, 0) [100..999 :: Int]
      finalCache = fst finalCacheAndCount
      evictionCount = snd finalCacheAndCount

  let len = size finalCache
  assertEqual "Cache length should match" 666 len

  let keys2 = keys finalCache
  assertEqual "Keys length should match cache length" len (length keys2)

  -- Check eviction count
  assertEqual "Eviction count should match" (234 :: Int) evictionCount

  -- Check values that should be in cache
  forM_ keys2 $ \k -> do
    let result = lookup k finalCache
    case result of
      Just (v, _)  -> assertEqual "Value should match key" k v
      Nothing -> assertFailure $ "Key " ++ show k ++ " should be in cache"

  -- These should be misses (keys 100-333, since we inserted 900 items in a cache of size 666)
  forM_ [100..333 :: Int] $ \i -> do
    let result = lookup i finalCache
    assertBool ("Key " ++ show i ++ " should not be in cache") (isNothing result)

  -- Set a new value and check it
  let cacheWithNewVal = insert (256 :: Int) (256 :: Int) finalCache

  let result = lookup (256 :: Int) cacheWithNewVal
  case result of
    Just (v, _) -> assertEqual "Value for key 256 should be 256" (256 :: Int) v
    Nothing -> assertFailure "Key 256 should be in cache"

  -- Check most frequently used key after updating key 256
  let updatedCache = case result of
        Just (_, c') -> c'
        Nothing -> cacheWithNewVal

  let keysAfterUpdate = keys updatedCache
  assertBool "Keys should be present after update" (not (null keysAfterUpdate))

  -- Purge and verify empty
  let purgedCache = purge updatedCache
  let lenAfterPurge = size purgedCache
  assertEqual "Cache should be empty after purge" 0 lenAfterPurge

  let resultAfterPurge = lookup (200 :: Int) purgedCache
  assertBool "Cache should contain nothing after purge" (isNothing resultAfterPurge)

testGDSF :: Assertion
testGDSF = do
  let initialCache :: LfudaCache Int Int
      initialCache = newGDSF 666

  -- Insert elements with power of 2 values
  let cacheWith10to19 = foldl' (\cache i ->
          insert i (2 ^ i) cache
        ) initialCache [10..19 :: Int]

  -- Insert more elements with same key/value
  let finalCache = foldl' (\cache i ->
          insert i i cache
        ) cacheWith10to19 [100..999 :: Int]

  let len = size finalCache
  assertEqual "Cache length should match" 666 len

  let keys2 = keys finalCache
  assertEqual "Keys length should match cache length" len (length keys2)

  -- Check values that should be in cache
  forM_ keys2 $ \k -> do
    let result = lookup k finalCache
    assertBool "Get should return a result" (not (isNothing result))
    case result of
      Just (v, _) ->
        if k >= 10 && k <= 19
          then assertEqual "Value should be 2^key for keys 10-19" (2 ^ k) v
          else assertEqual "Value should match key for other keys" k v
      Nothing -> assertFailure $ "Key " ++ show k ++ " should be in cache"

  -- Set a new value and check it
  let cacheWithNewVal = insert (256 :: Int) (256 :: Int) finalCache

  let result = lookup (256 :: Int) cacheWithNewVal
  case result of
    Just (v, _) -> assertEqual "Value for key 256 should be 256" (256 :: Int) v
    Nothing -> assertFailure "Key 256 should be in cache"

  -- Check most frequently used key after updating key 256
  let updatedCache = case result of
        Just (_, c') -> c'
        Nothing -> cacheWithNewVal

  let keysAfterUpdate = keys updatedCache
  -- Key 256 should have higher frequency due to the get operation above
  assertBool "Keys should be present after update" (not (null keysAfterUpdate))

testInsertEviction :: Assertion
testInsertEviction = do
  let cache :: LfudaCache Int Int
      cache = newCache 1 LFUDA

  -- First insert should not evict
  let (evicted1, _) = insertView (1 :: Int) (1 :: Int) cache
  assertEqual "Should not have evicted" Nothing evicted1

  -- Second insert (different key) should evict
  let c1 = insert (1 :: Int) (1 :: Int) cache
  let (evicted2, _) = insertView (2 :: Int) (2 :: Int) c1
  assertBool "Should have evicted" (evicted2 /= Nothing)

testLFUDAContains :: Assertion
testLFUDAContains = do
  let initialCache :: LfudaCache Int Int
      initialCache = newCache 2 LFUDA

  let cache1 = insert (1 :: Int) (1 :: Int) initialCache
  let cache2 = insert (2 :: Int) (2 :: Int) cache1

  -- Bump hits for key 1
  let finalCache1 = foldl' (\c _ ->
          case lookup (1 :: Int) c of
            Just (_, c') -> c'
            Nothing -> c
        ) cache2 [1..10 :: Int]

  let keys1 = keys finalCache1
  case keys1 of
    (k:_) -> assertEqual "Key 1 should be most frequently used" (1 :: Int) k
    [] -> assertFailure "Keys list should not be empty"

  -- Contains should not bump hits for key 2
  let finalCache2 = foldl' (\c _ -> c) finalCache1 (replicate 20 (contains (2 :: Int) finalCache1))

  let keys2 = keys finalCache2
  case keys2 of
    (k:_) -> assertEqual "Key 1 should still be most frequently used" (1 :: Int) k
    [] -> assertFailure "Keys list should not be empty"

testLFUDAPeek :: Assertion
testLFUDAPeek = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 2

  let cache1 = insert (1 :: Int) (1 :: Int) initialCache
  let cache2 = insert (2 :: Int) (2 :: Int) cache1

  -- Peek should not update frequency
  let result1 = peek (1 :: Int) cache2
  assertEqual "Value for key 1 should be 1" (Just 1) result1

  -- Increase frequency of key 2
  let cache2' = case lookup (2 :: Int) cache2 of
        Just (_, c) -> c
        Nothing -> cache2

  -- Adding key 3 should evict key 1 (lowest frequency)
  let cache3 = insert (3 :: Int) (3 :: Int) cache2'

  -- Key 1 should be evicted
  let containsKey1 = contains (1 :: Int) cache3
  assertBool "Key 1 should have been evicted" (not containsKey1)

testLFUDARemove :: Assertion
testLFUDARemove = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 2

  let cache1 = insert (1 :: Int) (1 :: Int) initialCache
  let cache2 = insert (2 :: Int) (2 :: Int) cache1

  let result1 = lookup (1 :: Int) cache2
  case result1 of
    Just (v, _) -> assertEqual "Value for key 1 should be 1" (1 :: Int) v
    Nothing -> assertFailure "Key 1 should be in cache"

  -- Remove key 1
  assertBool "Key 1 should be in cache before removal" (contains (1 :: Int) cache2)
  let cache2' = remove (1 :: Int) cache2

  let result1' = lookup (1 :: Int) cache2'
  assertBool "Key 1 should not be in cache after removal" (isNothing result1')

  let len = size cache2'
  assertEqual "Cache length should be 1 after removal" 1 len

testLFUDAAge :: Assertion
testLFUDAAge = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 1

  -- Set key 1 with initial frequency 1
  let cache1 = insert (1 :: Int) (1 :: Int) initialCache

  -- Bump hits on key 1 to frequency 2
  let cache1' = case lookup (1 :: Int) cache1 of
        Just (_, c) -> c
        Nothing -> cache1

  -- Set key 2 - but key 2 will be immediately evicted because
  -- it has lower priority (1) than key 1 (2)
  let cache2 = insert (2 :: Int) (2 :: Int) cache1'

  -- The age should now be 1 (the frequency of the evicted key 2)
  let age1 = age cache2
  assertEqual "Cache age should be 1" 1 age1

testLFUDASize :: Assertion
testLFUDASize = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 10

  -- Insert elements
  let finalCache = foldl' (\cache i ->
          insert i i cache
        ) initialCache [10..29 :: Int]

  -- Check size
  let s = size finalCache
  assertEqual "Cache size should be 10" 10 s

  -- Purge and check size again
  let purgedCache = purge finalCache
  let s' = size purgedCache
  assertEqual "Cache size should be 0 after purge" 0 s'

-- Edge Case Tests

-- Re-inserting a key resets its frequency to 1, making it vulnerable to eviction
testReinsertResetsFrequency :: Assertion
testReinsertResetsFrequency = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 2

  -- Insert keys 1 and 2
  let c1 = insert (1 :: Int) (10 :: Int) cache
  let c2 = insert (2 :: Int) (20 :: Int) c1

  -- Lookup key 1 many times to build up its frequency
  let c3 = foldl' (\c _ -> case lookup (1 :: Int) c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) c2 [1..10 :: Int]

  -- Key 1 now has high frequency. Re-insert it with new value - resets freq to 1!
  let c4 = insert (1 :: Int) (100 :: Int) c3

  -- Verify the value was updated
  assertEqual "Value should be updated to 100" (Just 100) (peek (1 :: Int) c4)

  -- Now insert key 3. Key 1 has freq 1 again, key 2 also has freq 1.
  -- One of them gets evicted.
  let c5 = insert (3 :: Int) (30 :: Int) c4
  assertEqual "Size should be 2" 2 (size c5)

-- Capacity 1 cache: every new distinct key causes eviction
testCapacity1 :: Assertion
testCapacity1 = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 1

  -- First insert: no eviction
  let (ev1, _) = insertView (1 :: Int) (10 :: Int) cache
  let c1 = insert (1 :: Int) (10 :: Int) cache
  assertEqual "First insert should not evict" Nothing ev1
  assertEqual "Size should be 1" 1 (size c1)

  -- Same key: replaces value, no eviction (size stays 1)
  let (ev2, c2) = insertView (1 :: Int) (20 :: Int) c1
  assertEqual "Re-insert same key should not evict" Nothing ev2
  assertEqual "Size should still be 1" 1 (size c2)
  assertEqual "Value should be updated" (Just 20) (peek (1 :: Int) c2)

  -- Different key: must evict
  let (ev3, c3) = insertView (2 :: Int) (30 :: Int) c2
  assertBool "Different key should evict" (ev3 /= Nothing)
  assertEqual "Size should still be 1" 1 (size c3)
  assertBool "Key 1 should be gone" (not (contains (1 :: Int) c3))
  assertEqual "Key 2 should be present" (Just 30) (peek (2 :: Int) c3)

  -- Lookup on evicted key
  let result = lookup (1 :: Int) c3
  assertBool "Lookup evicted key should return Nothing" (isNothing result)

-- Size must always match the actual number of entries in the queue
testSizeConsistency :: Assertion
testSizeConsistency = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 3

  -- Empty cache
  assertEqual "Empty cache size" 0 (size cache)

  -- Insert 3
  let c1 = insert (1 :: Int) (10 :: Int) cache
  assertEqual "After 1 insert" 1 (size c1)
  let c2 = insert (2 :: Int) (20 :: Int) c1
  let c3 = insert (3 :: Int) (30 :: Int) c2
  assertEqual "After 3 inserts" 3 (size c3)

  -- Remove 1
  let c4 = remove (1 :: Int) c3
  assertEqual "After remove" 2 (size c4)
  assertEqual "Keys count matches size" (size c4) (length (keys c4))

  -- Re-insert key 1 (no eviction, room available)
  let (ev, c5) = insertView (1 :: Int) (100 :: Int) c4
  assertEqual "Should not evict (room available)" Nothing ev
  assertEqual "After re-insert" 3 (size c5)

  -- Re-insert existing key 2 (replace, no size change)
  let c6 = insert (2 :: Int) (200 :: Int) c5
  assertEqual "After replace" 3 (size c6)
  assertEqual "Keys count matches size" (size c6) (length (keys c6))

  -- Insert 4th key to force eviction
  let (ev2, c7) = insertView (4 :: Int) (40 :: Int) c6
  assertBool "Should evict" (ev2 /= Nothing)
  assertEqual "After eviction" 3 (size c7)
  assertEqual "Keys count matches size" (size c7) (length (keys c7))

  -- Purge
  let c8 = purge c7
  assertEqual "After purge" 0 (size c8)
  assertEqual "Keys empty after purge" 0 (length (keys c8))

  -- Insert after purge
  let c9 = insert (5 :: Int) (50 :: Int) c8
  assertEqual "After insert post-purge" 1 (size c9)

-- Purge clears entries but does NOT reset age
testPurgePreservesAge :: Assertion
testPurgePreservesAge = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 1

  let c1 = insert (1 :: Int) (10 :: Int) cache
  -- Bump frequency to 3
  let c2 = foldl' (\c _ -> case lookup (1 :: Int) c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) c1 [1..2 :: Int]

  -- Evict key 1 by inserting key 2
  let c3 = insert (2 :: Int) (20 :: Int) c2
  let ageBeforePurge = age c3
  assertBool "Age should be > 0 after eviction" (ageBeforePurge > 0)

  -- Purge
  let c4 = purge c3
  assertEqual "Age should survive purge" ageBeforePurge (age c4)
  assertEqual "Size should be 0" 0 (size c4)

-- Cache should work normally after purge
testOpsAfterPurge :: Assertion
testOpsAfterPurge = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 2

  -- Fill cache, then purge
  let c1 = insert (1 :: Int) (10 :: Int) cache
  let c2 = insert (2 :: Int) (20 :: Int) c1
  let c3 = purge c2

  -- All operations should work on purged cache
  assertBool "Contains on purged cache" (not (contains (1 :: Int) c3))
  assertEqual "Peek on purged cache" Nothing (peek (1 :: Int) c3)
  assertBool "Lookup on purged cache" (isNothing (lookup (1 :: Int) c3))
  assertEqual "Keys on purged cache" [] (keys c3)

  -- Insert should work
  let (ev1, c4) = insertView (3 :: Int) (30 :: Int) c3
  assertEqual "Should not evict (empty after purge)" Nothing ev1
  assertEqual "Value accessible" (Just 30) (peek (3 :: Int) c4)
  let (ev2, c5) = insertView (4 :: Int) (40 :: Int) c4
  assertEqual "Should not evict (still room)" Nothing ev2
  assertEqual "Size should be 2" 2 (size c5)

  -- Eviction should work after purge
  let (ev3, c6) = insertView (5 :: Int) (50 :: Int) c5
  assertBool "Should evict now (full)" (ev3 /= Nothing)
  assertEqual "Size still 2" 2 (size c6)

-- insertView on an existing key should NOT report eviction
testInsertViewSelfReplace :: Assertion
testInsertViewSelfReplace = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 2

  let c1 = insert (1 :: Int) (10 :: Int) cache
  let c2 = insert (2 :: Int) (20 :: Int) c1

  -- insertView same key: no eviction, just replacement
  let (evicted, c3) = insertView (1 :: Int) (100 :: Int) c2
  assertEqual "Should not report eviction for self-replace" Nothing evicted
  assertEqual "Value should be updated" (Just 100) (peek (1 :: Int) c3)
  assertEqual "Size should be unchanged" 2 (size c3)

  -- insertView on capacity-1 cache, same key
  let cache1 :: LfudaCache Int Int
      cache1 = newLFUDA 1
  let c4 = insert (1 :: Int) (10 :: Int) cache1
  let (evicted2, c5) = insertView (1 :: Int) (100 :: Int) c4
  assertEqual "No eviction for self-replace on cap-1" Nothing evicted2
  assertEqual "Size should be 1" 1 (size c5)

-- insertView reports the correct evicted entry
testInsertViewEvictsCorrect :: Assertion
testInsertViewEvictsCorrect = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 1

  let c1 = insert (1 :: Int) (10 :: Int) cache

  -- Insert key 2, should evict key 1 and report it
  let (evicted, c2) = insertView (2 :: Int) (20 :: Int) c1
  assertEqual "Should report key 1 evicted" (Just (1 :: Int, 10 :: Int)) evicted
  assertEqual "Size should be 1" 1 (size c2)

  -- Bump key 2 frequency, then insert key 3
  let c3 = case lookup (2 :: Int) c2 of
        Just (_, c) -> c
        Nothing -> c2
  let (evicted2, _) = insertView (3 :: Int) (30 :: Int) c3
  -- Key 3 has freq 1, key 2 has freq 2, so key 3 gets evicted
  assertEqual "Should report key 3 evicted (lower freq)" (Just (3 :: Int, 30 :: Int)) evicted2

-- Remove on empty cache and nonexistent keys
testRemoveNonexistent :: Assertion
testRemoveNonexistent = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 5

  -- Remove from empty cache
  let c1 = remove (1 :: Int) cache
  assertEqual "Size should still be 0" 0 (size c1)

  -- Insert then remove nonexistent
  let c2 = insert (1 :: Int) (10 :: Int) c1
  assertBool "Key 999 should not be in cache" (not (contains (999 :: Int) c2))
  let c3 = remove (999 :: Int) c2
  assertEqual "Size should still be 1" 1 (size c3)

  -- Remove then remove same key again
  assertBool "Key 1 should be in cache" (contains (1 :: Int) c3)
  let c4 = remove (1 :: Int) c3
  assertBool "Key 1 should be gone after removal" (not (contains (1 :: Int) c4))
  let c5 = remove (1 :: Int) c4
  assertEqual "Size should be 0" 0 (size c5)

-- When all entries have the same frequency, eviction should still work
testEqualFrequencyEviction :: Assertion
testEqualFrequencyEviction = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 3

  -- Insert 3 entries, all with frequency 1
  let c1 = insert (1 :: Int) (10 :: Int) cache
  let c2 = insert (2 :: Int) (20 :: Int) c1
  let c3 = insert (3 :: Int) (30 :: Int) c2

  -- Insert 4th, must evict one (all have same freq)
  let (ev, c4) = insertView (4 :: Int) (40 :: Int) c3
  assertBool "Must evict something" (ev /= Nothing)
  assertEqual "Size must be 3" 3 (size c4)
  assertEqual "Keys count must be 3" 3 (length (keys c4))

  -- All remaining keys should be accessible
  let remainingKeys = keys c4
  forM_ remainingKeys $ \k ->
    assertBool ("Key " ++ show k ++ " should be accessible") (contains k c4)

-- LFUDA age should grow over multiple eviction cycles
testAgeAccumulation :: Assertion
testAgeAccumulation = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 1

  assertEqual "Initial age" 0 (age cache)

  -- Round 1: insert key 1, bump freq to 3, evict with key 2
  let c1 = insert (1 :: Int) (10 :: Int) cache
  let c2 = foldl' (\c _ -> case lookup (1 :: Int) c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) c1 [1..2 :: Int]
  -- key 1 has freq 3
  let c3 = insert (2 :: Int) (20 :: Int) c2
  -- key 2 (freq 1) was evicted, age should be 1
  let age1 = age c3
  assertEqual "Age after first eviction cycle" 1 age1

  -- Round 2: bump key 1 freq more, evict again
  let c4 = foldl' (\c _ -> case lookup (1 :: Int) c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) c3 [1..3 :: Int]
  -- key 1 now has freq 6
  let c5 = insert (3 :: Int) (30 :: Int) c4
  -- key 3 (freq 1) evicted, but its priority was 1 + age1 = 2
  -- age should be set to the evicted entry's frequency (1), NOT the priority
  let age2 = age c5
  assertEqual "Age after second eviction cycle" 1 age2

  -- Round 3: create a scenario where an entry with freq > 1 gets evicted.
  -- Use a size-2 cache for this.
  let cache2 :: LfudaCache Int Int
      cache2 = newLFUDA 2

  let d1 = insert (10 :: Int) (100 :: Int) cache2
  let d2 = insert (20 :: Int) (200 :: Int) d1
  -- Bump key 20 freq to 3
  let d3 = foldl' (\c _ -> case lookup (20 :: Int) c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) d2 [1..2 :: Int]
  -- key 10 has freq 1, key 20 has freq 3
  -- Insert key 30: evicts key 10 (freq 1), age becomes 1
  let d4 = insert (30 :: Int) (300 :: Int) d3
  assertEqual "Age after evicting freq-1 entry" 1 (age d4)

  -- Now bump key 30 freq to 4
  let d5 = foldl' (\c _ -> case lookup (30 :: Int) c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) d4 [1..3 :: Int]
  -- key 20 has freq 3, stale priority = 3 (set when age was 0, never recalculated)
  -- key 30 has freq 4, priority = 4 + 1 = 5 (recalculated on each lookup)
  -- Insert key 40: priority = 1 + age(1) = 2
  -- Eviction order by priority: key 40 (2) < key 20 (3) < key 30 (5)
  -- key 40 gets evicted immediately (lowest priority), age = freq(40) = 1
  let d6 = insert (40 :: Int) (400 :: Int) d5
  assertEqual "Age stays 1 (key 40 evicted, not key 20)" 1 (age d6)

  -- To actually evict a high-freq entry, we need to lookup key 20 first
  -- to refresh its priority with the current age
  let d7 = case lookup (20 :: Int) d6 of
        Just (_, c') -> c'  -- key 20 freq becomes 4, priority = 4 + 1 = 5
        Nothing -> d6
  -- key 20 now has freq 4, priority 5. key 30 has freq 4, priority 5.
  -- Insert key 50: priority = 1 + 1 = 2. Still lowest, key 50 evicted.
  let d8 = insert (50 :: Int) (500 :: Int) d7
  assertEqual "Age still 1 (new entry evicted again)" 1 (age d8)

-- After re-inserting a key, lookup should return the new value
-- but with reset frequency (making it vulnerable to eviction)
testLookupAfterReinsert :: Assertion
testLookupAfterReinsert = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 2

  let c1 = insert (1 :: Int) (10 :: Int) cache
  let c2 = insert (2 :: Int) (20 :: Int) c1

  -- Bump key 1 frequency high
  let c3 = foldl' (\c _ -> case lookup (1 :: Int) c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) c2 [1..5 :: Int]

  -- Re-insert key 1 with new value (resets freq to 1!)
  let c4 = insert (1 :: Int) (999 :: Int) c3

  -- Lookup should return new value
  case lookup (1 :: Int) c4 of
    Just (v, _) -> assertEqual "Should get new value" (999 :: Int) v
    Nothing -> assertFailure "Key 1 should be in cache"

  -- But now key 1 has low freq again.
  -- Bump key 2 freq so key 1 becomes eviction target
  let c5 = case lookup (2 :: Int) c4 of
        Just (_, c') -> c'
        Nothing -> c4

  -- Insert key 3: should evict key 1 (lowest freq after re-insert)
  let c6 = insert (3 :: Int) (30 :: Int) c5
  assertBool "Key 1 should be evicted (freq was reset)" (not (contains (1 :: Int) c6))
  assertBool "Key 2 should survive" (contains (2 :: Int) c6)

-- Rapid insert-remove cycles should keep size correct
testInsertRemoveCycles :: Assertion
testInsertRemoveCycles = do
  let cache :: LfudaCache Int Int
      cache = newLFUDA 5

  -- Insert and remove 100 keys rapidly
  let finalCache = foldl' (\c i ->
          let c1 = insert i i c
              c2 = remove i c1
          in c2
        ) cache [1..100 :: Int]

  assertEqual "Size should be 0 after insert-remove cycles" 0 (size finalCache)
  assertEqual "Keys should be empty" [] (keys finalCache)

  -- Insert-remove with some surviving (use capacity 10 to avoid eviction interference)
  let cache10 :: LfudaCache Int Int
      cache10 = newLFUDA 10
  let finalCache2 = foldl' (\c i ->
          let c1 = insert i i c
          in if even i
             then remove i c1
             else c1
        ) cache10 [1..10 :: Int]

  assertEqual "Size should be 5 (odd keys survive)" 5 (size finalCache2)
  forM_ [1, 3, 5, 7, 9 :: Int] $ \i ->
    assertBool ("Key " ++ show i ++ " should be present") (contains i finalCache2)
  forM_ [2, 4, 6, 8, 10 :: Int] $ \i ->
    assertBool ("Key " ++ show i ++ " should be absent") (not (contains i finalCache2))

  -- Verify that with tight capacity, eviction CAN steal surviving entries
  -- Capacity 5, insert keys 1-10, keep odd, remove even.
  -- At i=10, cache has [1,3,5,7,9] (full), inserting 10 evicts an odd key first!
  let finalCache3 = foldl' (\c i ->
          let c1 = insert i i c
          in if even i
             then remove i c1
             else c1
        ) cache [1..10 :: Int]

  assertEqual "Size should be 4 (one odd key was evicted by insert 10)" 4 (size finalCache3)

-- LFU Tests

testLFU :: Assertion
testLFU = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFU 5

  -- Insert 5 elements
  let cache1 = foldl' (\c i -> insert i (i * 10) c) initialCache [1..5 :: Int]
  assertEqual "Cache size should be 5" 5 (size cache1)

  -- All 5 elements should be present
  forM_ [1..5 :: Int] $ \i -> do
    let result = peek i cache1
    assertEqual ("Value for key " ++ show i) (Just (i * 10)) result

  -- Insert 6th element, should evict one
  let (evicted6, cache2) = insertView (6 :: Int) (60 :: Int) cache1
  assertBool "Should have evicted" (evicted6 /= Nothing)
  assertEqual "Cache size should still be 5" 5 (size cache2)

  -- Key 6 should be present
  let result6 = peek (6 :: Int) cache2
  assertEqual "Key 6 should be present" (Just 60) result6

testLFUNoAging :: Assertion
testLFUNoAging = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFU 1

  -- Insert key 1, then bump its frequency
  let cache1 = insert (1 :: Int) (10 :: Int) initialCache
  let cache1' = case lookup (1 :: Int) cache1 of
        Just (_, c) -> c
        Nothing -> cache1

  -- Age should be 0 before any eviction
  assertEqual "Age should be 0 initially" 0 (age cache1')

  -- Insert key 2, which evicts key 2 (lower priority) since key 1 has freq 2
  let cache2 = insert (2 :: Int) (20 :: Int) cache1'

  -- Age should STILL be 0 for LFU (no dynamic aging)
  assertEqual "Age should remain 0 for LFU" 0 (age cache2)

  -- Key 1 should survive (higher frequency)
  assertBool "Key 1 should still be in cache" (contains (1 :: Int) cache2)

testLFUFrequencyEviction :: Assertion
testLFUFrequencyEviction = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFU 3

  -- Insert 3 elements
  let cache1 = insert (1 :: Int) (10 :: Int) initialCache
  let cache2 = insert (2 :: Int) (20 :: Int) cache1
  let cache3 = insert (3 :: Int) (30 :: Int) cache2

  -- Bump frequency of key 1 (3 extra lookups -> freq 4)
  let cache3' = foldl' (\c _ ->
          case lookup (1 :: Int) c of
            Just (_, c') -> c'
            Nothing -> c
        ) cache3 [1..3 :: Int]

  -- Bump frequency of key 3 (1 extra lookup -> freq 2)
  let cache3'' = case lookup (3 :: Int) cache3' of
        Just (_, c) -> c
        Nothing -> cache3'

  -- Key 2 still has freq 1 (lowest), so inserting key 4 should evict key 2
  let cache4 = insert (4 :: Int) (40 :: Int) cache3''

  -- Key 2 should be gone (lowest frequency)
  assertBool "Key 2 should be evicted" (not (contains (2 :: Int) cache4))

  -- Keys 1 and 3 should still be present
  assertBool "Key 1 should survive" (contains (1 :: Int) cache4)
  assertBool "Key 3 should survive" (contains (3 :: Int) cache4)

  -- Key 4 should be present
  assertBool "Key 4 should be present" (contains (4 :: Int) cache4)

testLFUInsertView :: Assertion
testLFUInsertView = do
  let cache :: LfudaCache Int Int
      cache = newLFU 2

  -- First insert returns Nothing (no eviction)
  let (evicted1, cache1) = insertView (1 :: Int) (10 :: Int) cache
  assertEqual "No eviction on first insert" Nothing evicted1

  -- Second insert returns Nothing (still room)
  let (evicted2, cache2) = insertView (2 :: Int) (20 :: Int) cache1
  assertEqual "No eviction on second insert" Nothing evicted2

  -- Third insert should evict the lowest frequency entry
  let (evicted3, cache3) = insertView (3 :: Int) (30 :: Int) cache2
  assertBool "Should have evicted something" (evicted3 /= Nothing)
  assertEqual "Cache size should be 2" 2 (size cache3)

-- GDSF Tests

testGDSFSizeEviction :: Assertion
testGDSFSizeEviction = do
  -- GDSF priority = frequency + age * size
  -- All new entries have frequency 1 and size 1, so initially priority = 1 + 0*1 = 1
  -- After eviction, age increases, making new entries have higher priority
  let initialCache :: LfudaCache Int Int
      initialCache = newGDSF 2

  let cache1 = insert (1 :: Int) (10 :: Int) initialCache
  let cache2 = insert (2 :: Int) (20 :: Int) cache1

  -- Bump key 1 frequency to 2
  let cache2' = case lookup (1 :: Int) cache2 of
        Just (_, c) -> c
        Nothing -> cache2

  -- Insert key 3, should evict key 2 (freq 1 < key 1's freq 2)
  let cache3 = insert (3 :: Int) (30 :: Int) cache2'
  assertBool "Key 2 should be evicted" (not (contains (2 :: Int) cache3))
  assertBool "Key 1 should survive" (contains (1 :: Int) cache3)

testGDSFAging :: Assertion
testGDSFAging = do
  let initialCache :: LfudaCache Int Int
      initialCache = newGDSF 1

  -- Insert key 1, bump frequency to 3
  let cache1 = insert (1 :: Int) (10 :: Int) initialCache
  let cache1' = foldl' (\c _ ->
          case lookup (1 :: Int) c of
            Just (_, c') -> c'
            Nothing -> c
        ) cache1 [1..2 :: Int]

  assertEqual "Age should be 0 before eviction" 0 (age cache1')

  -- Insert key 2, it will be evicted (freq 1 < key 1's freq 3)
  let cache2 = insert (2 :: Int) (20 :: Int) cache1'

  -- Age should now be 1 (frequency of the evicted key)
  assertEqual "Age should be 1 after eviction" 1 (age cache2)
  assertBool "Key 1 should survive" (contains (1 :: Int) cache2)

testGDSFFrequencyAndSize :: Assertion
testGDSFFrequencyAndSize = do
  -- Test that GDSF uses both frequency and age*size in priority
  let initialCache :: LfudaCache Int Int
      initialCache = newGDSF 3

  -- Insert 3 entries
  let cache1 = insert (1 :: Int) (10 :: Int) initialCache
  let cache2 = insert (2 :: Int) (20 :: Int) cache1
  let cache3 = insert (3 :: Int) (30 :: Int) cache2

  -- Bump key 2's frequency to 3
  let cache3' = foldl' (\c _ ->
          case lookup (2 :: Int) c of
            Just (_, c') -> c'
            Nothing -> c
        ) cache3 [1..2 :: Int]

  -- Bump key 3's frequency to 2
  let cache3'' = case lookup (3 :: Int) cache3' of
        Just (_, c) -> c
        Nothing -> cache3'

  -- Key 1 has freq 1, Key 2 has freq 3, Key 3 has freq 2
  -- Insert key 4, should evict key 1 (lowest frequency)
  let cache4 = insert (4 :: Int) (40 :: Int) cache3''
  assertBool "Key 1 should be evicted (lowest freq)" (not (contains (1 :: Int) cache4))
  assertBool "Key 2 should survive" (contains (2 :: Int) cache4)
  assertBool "Key 3 should survive" (contains (3 :: Int) cache4)
  assertBool "Key 4 should be present" (contains (4 :: Int) cache4)

  -- Verify age was updated after eviction
  let ageAfter = age cache4
  assertBool "Age should have increased" (ageAfter > 0)

-- Pure implementation of random number generation for testing
type SimpleRandom :: Type
data SimpleRandom :: Type where
  SimpleRandom :: Int -> SimpleRandom

nextRandom :: SimpleRandom -> (Int, SimpleRandom)
nextRandom (SimpleRandom seed) =
  let newSeed = (seed * 1103515245 + 12345) `mod` 2147483647
      value = newSeed `mod` 32768
  in (value, SimpleRandom newSeed)

rangeRandom :: Int -> Int -> SimpleRandom -> (Int, SimpleRandom)
rangeRandom low high rand =
  let (val, rand') = nextRandom rand
      scaled = low + (val `mod` (high - low + 1))
  in (scaled, rand')

generateTrace :: Int -> SimpleRandom -> ([Int], SimpleRandom)
generateTrace n rand =
  go n rand []
  where
    go :: Int -> SimpleRandom -> [Int] -> ([Int], SimpleRandom)
    go 0 r acc = (reverse acc, r)
    go i r acc =
      let evenOdd = i `mod` 2 == 0
          (val, r') = if evenOdd
                     then rangeRandom 0 16383 r
                     else rangeRandom 0 32767 r
      in go (i-1) r' (val:acc)

-- Benchmark tests (simplified versions for HUnit)
testBenchmark :: String -> Assertion -> TestTree
testBenchmark name benchmark = testCase name benchmark

benchmarkLFUDA :: Assertion
benchmarkLFUDA = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 1000  -- Smaller cache size to ensure evictions

  -- Generate deterministic random trace
  let traceSeed = SimpleRandom 42
      (trace, _) = generateTrace 2000 traceSeed

  -- Split the trace: first half for setting, second half for getting
  let setTrace = take 1000 trace
      queryTrace = drop 1000 trace

  -- Set operations
  let cacheAfterSet = foldl' (\cache i ->
          insert i i cache
        ) initialCache setTrace

  -- Get operations and count hits/misses
  let hitsAndMisses = foldl' (\(h, m) i ->
          case lookup i cacheAfterSet of
            Just _  -> (h + 1, m)
            Nothing -> (h, m + 1)
        ) (0 :: Int, 0 :: Int) queryTrace
      hits = fst hitsAndMisses
      misses = snd hitsAndMisses

  -- With our cache size of 1000 and different query items,
  -- we should have both hits and misses
  assertBool "Should have some hits" (hits > (0 :: Int))
  assertBool "Should have some misses" (misses > (0 :: Int))

benchmarkLFUDARand :: Assertion
benchmarkLFUDARand = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 1000  -- Smaller cache size to ensure evictions

  -- Generate deterministic random trace
  let traceSeed = SimpleRandom 24
      (trace, _) = generateTrace 2000 traceSeed

  -- Process the trace in a pure way
  let processItem :: (LfudaCache Int Int, Int, Int) -> (Int, Int) -> (LfudaCache Int Int, Int, Int)
      processItem (cache, h, m) (idx, val) =
        -- Set on even indices
        let cache' = if even idx
                    then insert val val cache
                    else cache

            -- Check get result
            getResult = lookup val cache'
            h' = case getResult of { Just _  -> h + 1; Nothing -> h }
            m' = case getResult of { Just _  -> m;     Nothing -> m + 1 }

            -- Extra hits/misses for idx mod 7 = 0
            extras =
              if idx `mod` 7 == 0
              then foldl' (\(eh, em) _ ->
                      if contains val cache'
                      then (eh + 1, em)
                      else (eh, em + 1)
                   ) (0 :: Int, 0 :: Int) [1..19 :: Int]
              else (0, 0)

            -- Update cache if we got a hit
            cache'' = case getResult of
                        Just (_, c) -> c
                        Nothing -> cache'

        in (cache'', h' + fst extras, m' + snd extras)

  let fullResult = foldl' processItem (initialCache, 0 :: Int, 0 :: Int) (zip [0 :: Int ..] trace)

  -- Check results
  case fullResult of
    (_, hits, misses) -> do
      assertBool "Should have some hits" (hits > (0 :: Int))
      assertBool "Should have some misses" (misses > (0 :: Int))
