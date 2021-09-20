module Helper.Misc where

import Control.Monad (unless)
import Data.List (intercalate, isInfixOf)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Test.QuickCheck

checkResult :: Result -> IO ()
checkResult r =
  unless (isSuccess r) (printFailing r >> exitWith (ExitFailure 1))
  where
    printFailing = putStr . ("Failing labels: " ++) . show . failingLabels

assertEqual :: (Eq a, Show a) => a -> a -> Property
assertEqual expected result = counterexample info assert
  where
    assert :: Bool
    assert = result == expected

    info :: String
    info = "Got " ++ show result ++ ", expected " ++ show expected

assertIn :: (Eq a, Show a) => [[a]] -> [a] -> Property
assertIn expected result = counterexample info assert
  where
    assert :: Bool
    assert = all (`isInfixOf` result) expected

    info :: String
    info =
      "Got " ++ show result ++ ", should contain all these elements: "
        ++ intercalate ", " (map show expected)

assertNotEqual :: (Eq a, Show a) => a -> a -> Property
assertNotEqual expected result = counterexample info assert
  where
    assert :: Bool
    assert = result /= expected

    info :: String
    info = "Got " ++ show result ++ ", should differ from " ++ show expected
