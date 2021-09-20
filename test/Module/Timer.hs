module Module.Timer where

import Helper.Instance ()
import Helper.Misc
import PomodoroBar.Session.Timer (DisplayContext (..), mkBarText, mkCliText, mkTimerDigit)
import PomodoroBar.Time (Second (..))
import Test.QuickCheck

testTimer :: IO ()
testTimer =
  quickCheckResult (label "Timer.hs" $ conjoin tests) >>= checkResult
  where
    tests =
      [ label "mkTimerDigit #1" $
          forAll (Second <$> chooseInt (0, 3599)) $ testTimerDigitLength 5,
        label "mkTimerDigit #2" $
          forAll (Second <$> chooseInt (3600, 10000)) $ testTimerDigitLength 8,
        label "mkTimerDigit #3" $ mkTimerDigit 3671 === "01:01:11",
        label "mkBarText #1" $
          forAll arbitrary $ testCreateBarText1 "whatever",
        label "mkBarText #2" $
          forAll arbitrary $ testCreateBarText2 "whatever",
        label "mkCliText" $
          forAll arbitrary $ testCreateCliText "whatever"
      ]

    testCreateCliText :: String -> DisplayContext -> Property
    testCreateCliText digit c =
      let c' = c {progressBar = "a", keyHint = "c"}
          result = mkCliText c' digit
       in assertIn [progressBar c', digit, keyHint c'] result

    testCreateBarText1 :: String -> DisplayContext -> Property
    testCreateBarText1 digit c =
      let result = mkBarText c {barStatusText = "test"} digit
       in assertEqual (barLabel c ++ "test") result

    testCreateBarText2 :: String -> DisplayContext -> Property
    testCreateBarText2 digit c =
      let result = mkBarText c {barStatusText = ""} digit
       in assertEqual (barLabel c ++ digit) result

    testTimerDigitLength :: Int -> Second -> Property
    testTimerDigitLength expected n =
      let result = length $ mkTimerDigit n
       in counterexample
            ( "Timer at " ++ show n ++ " should have length " ++ show expected
                ++ ", got "
                ++ show result
            )
            $ result == expected
