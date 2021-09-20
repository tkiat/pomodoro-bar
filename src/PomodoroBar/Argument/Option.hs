{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module PomodoroBar.Argument.Option
  ( CliOption (..),
    mkCliOptions,
  )
where

import Data.List (intercalate)
import Options.Applicative (FlagFields, Mod, OptionFields, Parser, ParserInfo, fullDesc, header, help, helper, info, long, metavar, short, showDefault, value, (<**>))
import PomodoroBar.Argument.Parser (parserBartype, parserBool, parserMinute, parserPosInt, parserString)
import PomodoroBar.Display (BarType (..))
import PomodoroBar.Time (Minute (..))
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)

data CliOption = CliOption
  { oWork :: Minute,
    oBreak :: Minute,
    oLongBreak :: Minute,
    oSessionNum :: Int,
    oCmdWork :: String,
    oCmdBreak :: String,
    oBarType :: BarType,
    oRecordRaw :: Bool,
    oRecordSummary :: Bool,
    oNumWeek :: Int,
    oVersion :: Bool
  }
  deriving (Show)

cliOptions :: Parser CliOption
cliOptions = do
  oWork <- parserMinute optionsWork
  oBreak <- parserMinute optionsBreak
  oLongBreak <- parserMinute optionsLongBreak
  oSessionNum <- parserPosInt optionsSessionNum
  oCmdWork <- parserString optionsCmdWork
  oCmdBreak <- parserString optionsCmdBreak
  oBarType <- parserBartype optionsBarType

  oRecordRaw <- parserBool optionsRaw
  oRecordSummary <- parserBool optionsRecord
  oNumWeek <- parserPosInt optionsNumWeek
  oVersion <- parserBool optionsVersion

  pure CliOption {..}
  where
    optionsWork :: Mod OptionFields Minute
    optionsWork = optLenCommon "work" "work" 'w' (Minute 25)

    optionsBreak :: Mod OptionFields Minute
    optionsBreak = optLenCommon "break" "break" 'b' (Minute 5)

    optionsLongBreak :: Mod OptionFields Minute
    optionsLongBreak = optLenCommon "long break" "longbreak" 'l' (Minute 15)

    optionsSessionNum :: Mod OptionFields Int
    optionsSessionNum =
      long "session" <> short 's' <> value 1 <> metavar "NUM" <> showDefault
        <> help "Session number on start"

    optionsCmdWork :: Mod OptionFields String
    optionsCmdWork =
      long "cmdwork" <> value "" <> metavar "CMD" <> showDefault
        <> help "Shell command to execute when work session ends"

    optionsCmdBreak :: Mod OptionFields String
    optionsCmdBreak =
      long "cmdbreak" <> value "" <> metavar "CMD" <> showDefault
        <> help "Like --cmdwork but for unskipped break session"

    optionsBarType :: Mod OptionFields BarType
    optionsBarType =
      long "bartype" <> value None <> metavar "BAR" <> showDefault
        <> help
          ( "Also update external bar ["
              ++ (intercalate ", " . map show $ [(minBound :: BarType) ..])
              ++ "]. May require additional settings"
          )

    optionsRaw :: Mod FlagFields Bool
    optionsRaw = long "raw" <> help "Show raw record in minutes"

    optionsRecord :: Mod FlagFields Bool
    optionsRecord =
      long "record" <> short 'r'
        <> help
          "Show last 4 weeks summary (add -n option to adjust number of weeks\
          \and -w option to adjust session length)"

    optionsNumWeek :: Mod OptionFields Int
    optionsNumWeek = short 'n' <> value 4 <> showDefault <> metavar "NUM"

    optionsVersion :: Mod FlagFields Bool
    optionsVersion = short 'v' <> long "version" <> help "Show version"

    optLenCommon a b c d =
      long b <> short c <> value d <> metavar "MIN" <> showDefault
        <> help ("Minutes per " ++ a ++ " session")

mkCliOptions :: IO (ParserInfo CliOption)
mkCliOptions = do
  home <- getXdgDirectory XdgData ""
  let headerDscp =
        "pomodoro-bar: A pausable and configurable Pomodoro Timer with \
        \stats. The record file is stored at $XDG_DATA_HOME/pomodoro-bar/\
        \record.json, where XDG_DATA_HOME is "
          ++ home
  pure $ info (cliOptions <**> helper) (fullDesc <> header headerDscp)
