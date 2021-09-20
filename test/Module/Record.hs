module Module.Record where

import Data.Aeson (decode, encode)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import Helper.Instance ()
import Helper.Misc
import PomodoroBar.Record.Internal (Record (..), mkUpdatedRecord, mkWeekSummary, prettifyRecordSummary, prettifyRecordSummaryOneRow, recordPath)
import PomodoroBar.Time (Week (..), mkDayNum)
import Test.QuickCheck
import Test.QuickCheck.Monadic

testRecord :: IO ()
testRecord =
  quickCheckResult (label "Record.hs" $ conjoin tests) >>= checkResult
  where
    tests =
      [ label "decode (encode record) == record" $
          forAll arbitrary testIsomorphic,
        label "mkUpdatedRecord" $ forAll arbitrary testCreateUpdatedRecord,
        label "prettifyRecordSummaryOneRow" $
          prettifyRecordSummaryOneRow [1, 2, 5] ["12", "12", "1"] == "12  12      1",
        label "mkWeekSummary #1" $
          mkWeekSummarySingleTest Nothing (replicate 8 ""),
        label "mkWeekSummary #2" $
          mkWeekSummarySingleTest
            (Just $ Week 25 200 25 25 25 25 25)
            ["1.0", "8.0", "1.0", "1.0", "1.0", "1.0", "1.0", "2.0"],
        label "prettifyRecordSummary #1" $
          prettifyRecordSummarySingleTest [] 2,
        label "prettifyRecordSummary #2" $
          prettifyRecordSummarySingleTest [["a", "b"]] 3,
        label "recordPath" recordPathSingleTest
      ]

    testIsomorphic :: Record -> Property
    testIsomorphic r = case decode (encode r) :: Maybe Record of
      Just a -> assertEqual a r
      Nothing -> counterexample "cannot decode at all" False

    testCreateUpdatedRecord :: Record -> Property
    testCreateUpdatedRecord (Record m) = assertEqual expected result
      where
        m' = Map.insert "monday" (Week 0 25 0 0 0 0 0) m
        (Record m'') = mkUpdatedRecord (Record m') "monday" (mkDayNum 7) 25
        result = Map.lookup "monday" m''
        expected = Just $ Week 0 25 0 0 0 0 25

    mkWeekSummarySingleTest :: Maybe Week -> [String] -> Property
    mkWeekSummarySingleTest w expected =
      assertEqual (mkWeekSummary 25 (mkDayNum 7) w) expected

    prettifyRecordSummarySingleTest :: [[String]] -> Int -> Property
    prettifyRecordSummarySingleTest a expected =
      assertEqual expected (length (prettifyRecordSummary a))

    recordPathSingleTest :: Property
    recordPathSingleTest = monadicIO $ do
      p <- run recordPath
      assert ("/record.json" `isInfixOf` p)
