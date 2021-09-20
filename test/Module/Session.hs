module Module.Session where

import Helper.Gen (genNaturalEven, genNaturalMult8Minus1, genNaturalOdd, genNaturalOddExceptMult8Minus1)
import Helper.Instance ()
import Helper.Misc (checkResult)
import PomodoroBar.Session.Internal (Command (..), Session (..), SessionType (..), mkProgressBar, mkSession)
import PomodoroBar.Time (Second (..))
import Test.QuickCheck

testSession :: IO ()
testSession = do
  quickCheckResult (label "Session.hs" $ conjoin tests) >>= checkResult
  where
    tests =
      [ label "wSec" $ forAll genNaturalEven $ testSeconds wSec,
        label "bSec" $
          forAll genNaturalOddExceptMult8Minus1 $ testSeconds bSec,
        label "lSec" $ forAll genNaturalMult8Minus1 $ testSeconds lSec,
        label "cmdW" $ forAll genNaturalEven $ testCommand cmdW,
        label "cmdB" $ forAll genNaturalOdd $ testCommand cmdB,
        label "Work" $ forAll genNaturalEven $ testType Work,
        label "Rest" $ forAll genNaturalOdd $ testType Rest,
        label "progressBar" $
          forAll genNaturalMult8Minus1 $ testProgressBar "w-b-w-b-w-b-w-[l]",
        label "sNum" $ forAll genNaturalEven testSessionNum
      ]

    (wSec, bSec, lSec) = (Second 25, Second 5, Second 15)
    (cmdW, cmdB) = (Command "cmd-w", Command "cmd-b")

    sessions = map (mkSession wSec bSec lSec cmdW cmdB) [1 ..]

    testCommand :: Command -> Int -> Property
    testCommand expected n =
      let result = sCommand (sessions !! n)
       in counterexample
            (counterTemplate "sCommand" n result expected)
            $ result == expected

    testProgressBar :: String -> Int -> Property
    testProgressBar expected n =
      let result = mkProgressBar $ sessions !! n
       in counterexample
            (counterTemplate "mkProgressBar" n result expected)
            $ result == expected

    testSeconds :: Second -> Int -> Property
    testSeconds expected n =
      let result = sSeconds (sessions !! n)
       in counterexample
            (counterTemplate "sSeconds" n (show result) (show expected))
            $ result == expected

    testSessionNum :: Int -> Property
    testSessionNum n =
      let cur = sNum (sessions !! n)
          next = sNum (sessions !! (n + 1))
       in counterexample
            ( "sessions at index " ++ show n ++ " and " ++ show (n + 1)
                ++ " should give the same session number"
            )
            $ cur == next

    testType :: SessionType -> Int -> Property
    testType expected n =
      let result = sType (sessions !! n)
       in counterexample
            (counterTemplate "sType" n (show result) (show expected))
            $ result == expected

    counterTemplate :: Show a => String -> Int -> a -> a -> String
    counterTemplate a b c d =
      a ++ " (sessions !! " ++ show b ++ ") gives " ++ show c ++ ", expect "
        ++ show d
