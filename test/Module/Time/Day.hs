module Module.Time.Day where

import Helper.Misc
import PomodoroBar.Time (getDayNumber, getNthMondayUntilNow)
import PomodoroBar.Time.Day (unDayNum)
import Test.QuickCheck
import Test.QuickCheck.Monadic

testTimeDay :: IO ()
testTimeDay =
  quickCheckResult (label "Time.hs > Day" $ conjoin tests) >>= checkResult
  where
    tests =
      [ label "getDayNumber" testDayNumberInWeek,
        label "getNthMondayUntilNow" testMonday
      ]

    testDayNumberInWeek = monadicIO $ do
      n <- run getDayNumber
      let d = unDayNum n in assert (d > 0 && d < 8)

    testMonday = monadicIO $ do
      s <- run $ getNthMondayUntilNow 1
      assert (s /= "")
