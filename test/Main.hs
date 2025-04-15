module Main (main) where

import Test.Data.LfudaCache (tests)
import Test.Tasty

main :: IO ()
main = defaultMain tests
