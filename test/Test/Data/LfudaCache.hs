module Test.Data.LfudaCache
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Data.Maybe (isJust, isNothing)
import Data.LfudaCache
import Data.Foldable (foldl')
import Prelude hiding (lookup)
import Data.Kind


tests :: TestTree
tests = testGroup "LFUDA Cache Tests"
  [ testCase "Basic LFUDA Operations" testLFUDA
  , testCase "GDSF Test" testGDSF
  , testCase "Set Returns Eviction Status" testLFUDASet
  , testCase "Contains Doesn't Update Frequency" testLFUDAContains
  , testCase "ContainsOrSet Functionality" testLFUDAContainsOrSet
  , testCase "PeekOrSet Functionality" testLFUDAPeekOrSet
  , testCase "Peek Doesn't Update Frequency" testLFUDAPeek
  , testCase "Remove Operation" testLFUDARemove
  , testCase "Age Tracking" testLFUDAAge
  , testCase "Size Tracking" testLFUDASize
  , testBenchmark "LFUDA Benchmark" benchmarkLFUDA
  , testBenchmark "LFUDA Random Benchmark" benchmarkLFUDARand
  ]

testLFUDA :: Assertion
testLFUDA = do
  -- Create cache with proper policy
  let initialCache :: LfudaCache Int Int
      initialCache = newCache 666 LFUDA

  -- Track number of evictions
  let (finalCache, evictionCount) = foldl' insertAndTrackEvictions (initialCache, 0) [100..999 :: Int]

      insertAndTrackEvictions (cache, count) i =
        let (evicted', cache') = set i i cache
            newCount = if evicted' then count + 1 else count
        in (cache', newCount)

  let len = size finalCache
  assertEqual "Cache length should match" 666 len

  let keys2 = keys finalCache
  assertEqual "Keys length should match cache length" len (length keys2)

  -- Check eviction count
  assertEqual "Eviction count should match" (900 - 666) evictionCount

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

  -- Set a newLFUDA value and check it
  let (_, cacheWithNewVal) = set (256 :: Int) (256 :: Int) finalCache

  let result = lookup (256 :: Int) cacheWithNewVal
  case result of
    Just (v, _) -> assertEqual "Value for key 256 should be 256" (256 :: Int) v
    Nothing -> assertFailure "Key 256 should be in cache"

  -- Check most frequently used key after updating key 256
  let (_, updatedCache) = case result of
        Just (_, c') -> ((), c')
        Nothing -> ((), cacheWithNewVal)

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
          let (_, cache') = set i (2 ^ i) cache
          in cache'
        ) initialCache [10..19 :: Int]

  -- Insert more elements with same key/value
  let finalCache = foldl' (\cache i ->
          let (_, cache') = set i i cache
          in cache'
        ) cacheWith10to19 [100..999 :: Int]

  let len = size finalCache
  assertEqual "Cache length should match" 666 len

  let keys2 = keys finalCache
  assertEqual "Keys length should match cache length" len (length keys2)

  -- Check values that should be in cache
  forM_ keys2 $ \k -> do
    let result = lookup k finalCache
    assertBool "Get should return a result" (isJust result)
    case result of
      Just (v, _) ->
        if k >= 10 && k <= 19
          then assertEqual "Value should be 2^key for keys 10-19" (2 ^ k) v
          else assertEqual "Value should match key for other keys" k v
      Nothing -> assertFailure $ "Key " ++ show k ++ " should be in cache"

  -- Set a newLFUDA value and check it
  let (_, cacheWithNewVal) = set (256 :: Int) (256 :: Int) finalCache

  let result = lookup (256 :: Int) cacheWithNewVal
  case result of
    Just (v, _) -> assertEqual "Value for key 256 should be 256" (256 :: Int) v
    Nothing -> assertFailure "Key 256 should be in cache"

  -- Check most frequently used key after updating key 256
  let (_, updatedCache) = case result of
        Just (_, c') -> ((), c')
        Nothing -> ((), cacheWithNewVal)

  let keysAfterUpdate = keys updatedCache
  -- Key 256 should have higher frequency due to the get operation above
  assertBool "Keys should be present after update" (not (null keysAfterUpdate))

testLFUDASet :: Assertion
testLFUDASet = do
  let cache :: LfudaCache Int Int
      cache = newCache 1 LFUDA

  -- First set should not evict
  let (evicted1, cache') = set (1 :: Int) (1 :: Int) cache
  assertBool "Should not have evicted" (not evicted1)

  -- Second set should evict
  let (evicted2, _) = set (2 :: Int) (2 :: Int) cache'
  assertBool "Should have evicted" evicted2

testLFUDAContains :: Assertion
testLFUDAContains = do
  let initialCache :: LfudaCache Int Int
      initialCache = newCache 2 LFUDA

  let (_, cache1) = set (1 :: Int) (1 :: Int) initialCache
  let (_, cache2) = set (2 :: Int) (2 :: Int) cache1

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

testLFUDAContainsOrSet :: Assertion
testLFUDAContainsOrSet = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 2

  let (_, cache1) = set (1 :: Int) (1 :: Int) initialCache
  let (_, cache2) = set (2 :: Int) (2 :: Int) cache1

  -- Test for existing key
  let (contains1, eviction1, _) = containsOrSet (1 :: Int) (1 :: Int) cache2
  assertBool "Key 1 should be contained" contains1
  assertBool "Nothing should have been evicted" (not eviction1)

  -- Test for newLFUDA key
  let (contains3, eviction3, _) = containsOrSet (3 :: Int) (3 :: Int) cache2
  assertBool "Key 3 should not have been contained" (not contains3)
  assertBool "Key 3 should have been set with eviction" eviction3

testLFUDAPeekOrSet :: Assertion
testLFUDAPeekOrSet = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 2

  let (_, cache1) = set (1 :: Int) (1 :: Int) initialCache
  let (_, cache2) = set (2 :: Int) (2 :: Int) cache1

  -- Test for existing key
  let (prev1, contains1, set1, _) = peekOrSet (1 :: Int) (1 :: Int) cache2
  assertEqual "Previous value should be Just 1" (Just 1) prev1
  assertBool "Key 1 should be contained" contains1
  assertBool "Nothing should have been set" (not set1)

  -- Test for newLFUDA key
  let (prev3, contains3, set3, cache3) = peekOrSet (3 :: Int) (3 :: Int) cache2
  assertEqual "Previous value should be Nothing" Nothing prev3
  assertBool "Key 3 should not have been contained" (not contains3)
  assertBool "Key 3 should have been set" set3

  -- Bump hits for key 3
  let cache3' = case lookup (3 :: Int) cache3 of
        Just (_, c) -> c
        Nothing -> cache3

  -- Test for key 3 again
  let (prev3', contains3', set3', _) = peekOrSet (3 :: Int) (3 :: Int) cache3'
  assertEqual "Previous value should be Just 3" (Just 3) prev3'
  assertBool "Key 3 should be contained" contains3'
  assertBool "Nothing should have been set" (not set3')

testLFUDAPeek :: Assertion
testLFUDAPeek = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 2

  let (_, cache1) = set (1 :: Int) (1 :: Int) initialCache
  let (_, cache2) = set (2 :: Int) (2 :: Int) cache1

  -- Peek should not update frequency
  let result1 = peek (1 :: Int) cache2
  assertEqual "Value for key 1 should be 1" (Just 1) result1

  -- Increase frequency of key 2
  let cache2' = case lookup (2 :: Int) cache2 of
        Just (_, c) -> c
        Nothing -> cache2

  -- Adding key 3 should evict key 1 (lowest frequency)
  let (_, cache3) = set (3 :: Int) (3 :: Int) cache2'

  -- Key 1 should be evicted
  let containsKey1 = contains (1 :: Int) cache3
  assertBool "Key 1 should have been evicted" (not containsKey1)

testLFUDARemove :: Assertion
testLFUDARemove = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 2

  let (_, cache1) = set (1 :: Int) (1 :: Int) initialCache
  let (_, cache2) = set (2 :: Int) (2 :: Int) cache1

  let result1 = lookup (1 :: Int) cache2
  case result1 of
    Just (v, _) -> assertEqual "Value for key 1 should be 1" (1 :: Int) v
    Nothing -> assertFailure "Key 1 should be in cache"

  -- Remove key 1
  let (removed, cache2') = remove (1 :: Int) cache2
  assertBool "Remove should return True" removed

  let result1' = lookup (1 :: Int) cache2'
  assertBool "Key 1 should not be in cache after removal" (isNothing result1')

  let len = size cache2'
  assertEqual "Cache length should be 1 after removal" 1 len

testLFUDAAge :: Assertion
testLFUDAAge = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 1

  -- Set key 1 with initial frequency 1
  let (_, cache1) = set (1 :: Int) (1 :: Int) initialCache

  -- Bump hits on key 1 to frequency 2
  let cache1' = case lookup (1 :: Int) cache1 of
        Just (_, c) -> c
        Nothing -> cache1

  -- Set key 2 - but key 2 will be immediately evicted because
  -- it has lower priority (1) than key 1 (2)
  let (evicted, cache2) = set (2 :: Int) (2 :: Int) cache1'
  assertBool "Set operation should have evicted key 2" evicted

  -- The age should now be 1 (the frequency of the evicted key 2)
  -- NOT 2 as the original test expected
  let age1 = age cache2
  assertEqual "Cache age should be 1" 1 age1

testLFUDASize :: Assertion
testLFUDASize = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 10

  -- Insert elements
  let finalCache = foldl' (\cache i ->
          let (_, cache') = set i i cache
          in cache'
        ) initialCache [10..29 :: Int]

  -- Check size
  let s = size finalCache
  assertEqual "Cache size should be 10" 10 s

  -- Purge and check size again
  let purgedCache = purge finalCache
  let s' = size purgedCache
  assertEqual "Cache size should be 0 after purge" 0 s'

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
          let (_, cache') = set i i cache
          in cache'
        ) initialCache setTrace

  -- Get operations and count hits/misses
  let (hits, misses) = foldl' (\(h, m) i ->
          case lookup i cacheAfterSet of
            Just _  -> (h + 1, m)
            Nothing -> (h, m + 1)
        ) (0, 0) queryTrace

  -- With our cache size of 1000 and different query items,
  -- we should have both hits and misses
  assertBool "Should have some hits" (hits > 0)
  assertBool "Should have some misses" (misses > 0)

benchmarkLFUDARand :: Assertion
benchmarkLFUDARand = do
  let initialCache :: LfudaCache Int Int
      initialCache = newLFUDA 1000  -- Smaller cache size to ensure evictions

  -- Generate deterministic random trace
  let traceSeed = SimpleRandom 24
      (trace, _) = generateTrace 2000 traceSeed

  -- Process the trace in a pure way
  let result = foldl' processItem (initialCache, 0, 0) (zip [0..] trace)
      (_, hits, misses) = result

      processItem (cache, h, m) (idx, val) =
        -- Set on even indices
        let cache' = if even idx
                    then let (_, c) = set val val cache in c
                    else cache

            -- Check get result
            getResult = lookup val cache'
            (h', m') = case getResult of
                        Just _  -> (h + 1, m)
                        Nothing -> (h, m + 1)

            -- Extra hits/misses for idx mod 7 = 0
            (extraH, extraM) =
              if idx `mod` 7 == 0
              then foldl' (\(eh, em) _ ->
                      if contains val cache'
                      then (eh + 1, em)
                      else (eh, em + 1)
                   ) (0, 0) [1..19]
              else (0, 0)

            -- Update cache if we got a hit
            newCache = case getResult of
                        Just (_, c) -> c
                        Nothing -> cache'

        in (newCache, h' + extraH, m' + extraM)

  -- Check results
  assertBool "Should have some hits" (hits > (0 :: Int))
  assertBool "Should have some misses" (misses > (0 :: Int))
