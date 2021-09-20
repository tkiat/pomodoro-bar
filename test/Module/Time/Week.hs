{-# LANGUAGE RecordWildCards #-}

module Module.Time.Week where

import Data.Aeson (decode, encode)
import Data.Either (isLeft)
import Helper.Instance ()
import Helper.Misc
import PomodoroBar.Time (Minute (..), Week (..), addSessionToWeek, mkDayNum, mkExistingWeekSummary)
import PomodoroBar.Time.Week (mkWeek)
import Test.QuickCheck

testTimeWeek :: IO ()
testTimeWeek =
  quickCheckResult (label "Time.hs > Week" $ conjoin tests) >>= checkResult
  where
    tests =
      [ label "decode (encode week) == week" $ forAll arbitrary testIsomorphic,
        label "mkWeek" $ forAll (genListMinute 7) testCreateWeek,
        label "mkWeek" $ forAll (genListMinute 6) testCreateWeek,
        label "addSessionToWeek" addSessionToWeekSingleTest,
        label "mkExistingWeekSummary" mkExistingWeekSummarySingleTest
      ]

    testIsomorphic :: Week -> Property
    testIsomorphic w = case decode (encode w) :: Maybe Week of
      Just a -> counterexample "cannot encode or decode correctly" (a == w)
      Nothing -> counterexample "cannot decode at all" False

    testCreateWeek :: [Minute] -> Property
    testCreateWeek a@[mon, tue, wed, thu, fri, sat, sun] =
      assertEqual (Right Week {..}) (mkWeek a)
    testCreateWeek a =
      counterexample "should be Left, got Right" (isLeft $ mkWeek a)

    week1 = Week 25 200 25 25 25 25 25
    week2 = week1 {sun = 50}

    addSessionToWeekSingleTest :: Bool
    addSessionToWeekSingleTest =
      addSessionToWeek week1 (Minute 25) (mkDayNum 7) == week2

    mkExistingWeekSummarySingleTest = expect2 === result2
      where
        expect2 = ["1.0", "8.0", "1.0", "1.0", "1.0", "1.0", "1.0", "2.0"]
        result2 = mkExistingWeekSummary (mkDayNum 7) (Minute 25) week1

    genListMinute :: Int -> Gen [Minute]
    genListMinute a = vectorOf a $ Minute <$> choose (0, 250)
