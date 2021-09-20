module Module.PomodoroBar where

import Helper.Instance ()
import Helper.Misc
import PomodoroBar (parseArgs)
import Test.QuickCheck
import Test.QuickCheck.Monadic

testUserChoice :: IO ()
testUserChoice =
  quickCheckResult (label "PomodoroBar.hs" $ conjoin tests) >>= checkResult
  where
    tests =
      [label "parseArgs" testNoResultPomodoroBar]

    -- just check if running without error
    testNoResultPomodoroBar :: Property
    testNoResultPomodoroBar = monadicIO $ do
      _ <- run parseArgs
      assert True
