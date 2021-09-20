module Module.Time.Second where

import Helper.Instance ()
import Helper.Misc
import PomodoroBar.Time (Minute (..), Second, toMin, toSecond)
import Test.QuickCheck

testTimeSecond :: IO ()
testTimeSecond =
  quickCheckResult (label "Time.hs > Second" $ conjoin tests) >>= checkResult
  where
    tests =
      [ label "toMin . toSecond" $ forAll arbitrary testMinuteIsomorphic,
        label "toSecond . toMin" $ forAll arbitrary testSecondIsomorphic
      ]
    testMinuteIsomorphic :: Minute -> Property
    testMinuteIsomorphic m = assertEqual m (toMin $ toSecond m)

    testSecondIsomorphic :: Second -> Property
    testSecondIsomorphic s = assertEqual (s * 60) (toSecond $ toMin (s * 60))
