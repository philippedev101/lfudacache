module Main (main) where

import Test.Tasty.Bench
import Data.LfudaCache
import Data.Foldable (foldl')
import Prelude hiding (lookup)


main :: IO ()
main = defaultMain
  [ bgroup "insert"
    [ bench "LFUDA/1000" $ nf (insertN LFUDA 1000) 10000
    , bench "GDSF/1000"  $ nf (insertN GDSF 1000)  10000
    , bench "LFU/1000"   $ nf (insertN LFU 1000)    10000
    ]
  , bgroup "lookup"
    [ bench "LFUDA/hit"  $ nf (lookupHit LFUDA 1000)  5000
    , bench "GDSF/hit"   $ nf (lookupHit GDSF 1000)   5000
    , bench "LFU/hit"    $ nf (lookupHit LFU 1000)    5000
    , bench "LFUDA/miss" $ nf (lookupMiss LFUDA 1000)  5000
    , bench "LFU/miss"   $ nf (lookupMiss LFU 1000)   5000
    ]
  , bgroup "mixed"
    [ bench "LFUDA/1000" $ nf (mixedWorkload LFUDA 1000) 10000
    , bench "GDSF/1000"  $ nf (mixedWorkload GDSF 1000)  10000
    , bench "LFU/1000"   $ nf (mixedWorkload LFU 1000)   10000
    ]
  , bgroup "contains"
    [ bench "LFUDA/1000" $ nf (containsWorkload LFUDA 1000) 5000
    , bench "LFU/1000"   $ nf (containsWorkload LFU 1000)   5000
    ]
  ]

-- | Insert n items into a cache of given capacity
insertN :: CachePolicy -> Int -> Int -> LfudaCache Int Int
insertN policy capacity n =
  foldl' (\c i -> insert i i c) (newCache capacity policy) [0..n-1]

-- | Lookup items that are in the cache (hits)
lookupHit :: CachePolicy -> Int -> Int -> LfudaCache Int Int
lookupHit policy capacity n =
  let cache0 = foldl' (\c i -> insert i i c) (newCache capacity policy) [0..capacity-1]
  in foldl' (\c i ->
        case lookup (i `mod` capacity) c of
          Just (_, c') -> c'
          Nothing -> c
      ) cache0 [0..n-1]

-- | Lookup items that are not in the cache (misses)
lookupMiss :: CachePolicy -> Int -> Int -> LfudaCache Int Int
lookupMiss policy capacity n =
  let cache0 = foldl' (\c i -> insert i i c) (newCache capacity policy) [0..capacity-1]
  in foldl' (\c i ->
        case lookup (capacity + i) c of
          Just (_, c') -> c'
          Nothing -> c
      ) cache0 [0..n-1]

-- | Mixed insert/lookup workload
mixedWorkload :: CachePolicy -> Int -> Int -> LfudaCache Int Int
mixedWorkload policy capacity n =
  foldl' (\c i ->
    if even i
    then insert (i `mod` (capacity * 2)) i c
    else case lookup (i `mod` (capacity * 2)) c of
           Just (_, c') -> c'
           Nothing -> c
  ) (newCache capacity policy) [0..n-1]

-- | Contains check workload (no frequency update)
containsWorkload :: CachePolicy -> Int -> Int -> Int
containsWorkload policy capacity n =
  let cache0 = foldl' (\c i -> insert i i c) (newCache capacity policy) [0..capacity-1]
  in foldl' (\acc i ->
        if contains (i `mod` (capacity * 2)) cache0
        then acc + 1
        else acc
      ) (0 :: Int) [0..n-1]
