module Helper.Gen where

import Test.QuickCheck

genNaturalEven :: Gen Int
genNaturalEven = arbitrarySizedNatural `suchThat` even

genNaturalOdd :: Gen Int
genNaturalOdd = arbitrarySizedNatural `suchThat` odd

genNaturalOddExceptMult8Minus1 :: Gen Int
genNaturalOddExceptMult8Minus1 =
  genNaturalOdd `suchThat` (\x -> (x + 1) `mod` 8 /= 0)

genNaturalMult8Minus1 :: Gen Int
genNaturalMult8Minus1 =
  arbitrarySizedNatural `suchThat` (\x -> (x + 1) `mod` 8 == 0)
