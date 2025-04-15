module Main (main) where

import Test.Data.LfudaCache (tests)
import Test.Tasty
import Prelude


main :: IO ()
main = defaultMain tests
