{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Helper.Instance where

import qualified Data.Map as Map
import PomodoroBar.Argument.Option (CliOption (..))
import PomodoroBar.Display (BarType (..))
import PomodoroBar.Record.Internal (Record (..))
import PomodoroBar.Session.Internal (Command (..))
import PomodoroBar.Session.Timer (DisplayContext (..))
import PomodoroBar.Time (Minute (..), Second (..), Week (..))
import Test.QuickCheck

deriving instance Eq Command

deriving instance Show Command

deriving instance Show DisplayContext

deriving instance Eq Record

deriving instance Show Record

deriving instance Show Week

instance Arbitrary BarType where
  arbitrary = elements [Xmobar, Polybar, None]

instance Arbitrary Second where
  arbitrary = Second . getPositive <$> arbitrary

instance Arbitrary Minute where
  arbitrary = Minute . getPositive <$> arbitrary

instance Arbitrary CliOption where
  arbitrary = do
    oWork <- arbitrary
    oBreak <- arbitrary
    oLongBreak <- arbitrary
    oSessionNum <- getPositive <$> arbitrary
    oCmdWork <- arbitrary
    oCmdBreak <- arbitrary
    oBarType <- arbitrary
    oRecordRaw <- arbitrary
    oRecordSummary <- arbitrary
    oNumWeek <- getPositive <$> arbitrary
    oVersion <- arbitrary

    pure $ CliOption {..}

instance Arbitrary DisplayContext where
  arbitrary = do
    barIsWorkMode <- arbitrary
    barLabel <- arbitrary
    barStatusText <- arbitrary
    barType <- arbitrary
    keyHint <- arbitrary
    progressBar <- arbitrary
    second <- arbitrary

    pure $ DisplayContext {..}

instance Arbitrary Record where
  arbitrary = Record <$> (arbitrary :: Gen (Map.Map String Week))

instance Arbitrary Week where
  arbitrary = Week <$> m <*> m <*> m <*> m <*> m <*> m <*> m
    where
      m = arbitrary
