module Main (main) where

import Data.LfudaCache
import Data.Foldable (foldl')
import Prelude hiding (lookup)


main :: IO ()
main = do
  putStrLn "=== LFUDA Cache Demo ==="
  putStrLn ""

  -- Create a small cache to demonstrate eviction behavior
  let cache :: LfudaCache String Int
      cache = newLFUDA 3
  putStrLn $ "Created LFUDA cache with capacity 3"
  putStrLn ""

  -- Insert some entries
  let c1 = insert "alice" (100 :: Int) cache
  let c2 = insert "bob"   200 c1
  let c3 = insert "carol" 300 c2
  putStrLn $ "Inserted alice=100, bob=200, carol=300"
  putStrLn $ "Size: " ++ show (size c3)
  putStrLn $ "Keys: " ++ show (keys c3)
  putStrLn ""

  -- Lookup bumps frequency
  putStrLn "Looking up 'alice' 3 times (bumps frequency)..."
  let c4 = foldl' (\c _ -> case lookup "alice" c of
                      Just (_, c') -> c'
                      Nothing -> c
                   ) c3 [1..3 :: Int]

  -- Insert a 4th entry - should evict lowest frequency entry
  -- Use insertView to see what was evicted
  let (evicted, c5) = insertView "dave" 400 c4
  putStrLn $ "Inserted dave=400, eviction occurred: " ++ show (evicted /= Nothing)
  case evicted of
    Just (k, v) -> putStrLn $ "Evicted: " ++ show k ++ "=" ++ show v
    Nothing     -> pure ()
  putStrLn $ "Keys after eviction: " ++ show (keys c5)
  putStrLn $ "alice still in cache: " ++ show (contains "alice" c5)
  putStrLn ""

  -- Demonstrate peek vs lookup
  putStrLn "--- Peek vs Lookup ---"
  case peek "alice" c5 of
    Just v  -> putStrLn $ "peek alice = " ++ show v ++ " (no frequency update)"
    Nothing -> putStrLn "alice not found"

  case lookup "alice" c5 of
    Just (v, _) -> putStrLn $ "lookup alice = " ++ show v ++ " (frequency updated)"
    Nothing     -> putStrLn "alice not found"
  putStrLn ""

  -- Demonstrate insertView
  putStrLn "--- InsertView ---"
  let (evictedEntry, c6) = insertView "eve" 500 c5
  case evictedEntry of
    Just (k, v) -> putStrLn $ "Evicted: " ++ show k ++ "=" ++ show v
    Nothing     -> putStrLn "No eviction"
  putStrLn $ "Keys: " ++ show (keys c6)
  putStrLn ""

  -- Compare policies
  putStrLn "=== Policy Comparison ==="
  putStrLn ""
  comparePolicies

comparePolicies :: IO ()
comparePolicies = do
  let capacity :: Int
      capacity = 3
      items :: [(String, Int)]
      items = [("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5)]

  -- For each policy, insert items and show what survives
  let policies :: [(CachePolicy, String)]
      policies = [(LFUDA, "LFUDA"), (GDSF, "GDSF"), (LFU, "LFU")]

  mapM_ (\(policy, name) -> do
    let cache0 :: LfudaCache String Int
        cache0 = newCache capacity policy
        -- Insert first 3
        cache1 = foldl' (\c (k, v) -> insert k v c) cache0 (take 3 items)
    -- Bump frequency of "a"
    let cache2 = foldl' (\c _ -> case lookup "a" c of
                            Just (_, c') -> c'
                            Nothing -> c
                         ) cache1 [1..2 :: Int]
    -- Insert remaining 2
    let cache3 = foldl' (\c (k, v) -> insert k v c) cache2 (drop 3 items)

    putStrLn $ name ++ ":"
    putStrLn $ "  Surviving keys: " ++ show (keys cache3)
    putStrLn $ "  Age: " ++ show (age cache3)
    ) policies
