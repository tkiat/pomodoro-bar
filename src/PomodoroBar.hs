-- TODO press s repeatly not good
-- TODO stuck at getchar both sigINT handler and CTRL+Z
-- TODO prevent store float in record, also changes this for python

module PomodoroBar where

import Data.List (elem, intersect, isInfixOf, isSuffixOf)
import Data.Semigroup ((<>))
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (ExitCode ( ExitFailure ), exitWith)

import Record.Record (ensureRecordExist, showRecordRaw, showRecordLast4Weeks)
import Timer.TimerManager (startTimerManager)

data Argument = Argument
  { work :: Int
  , break :: Int
  , longbreak :: Int
  , raw :: Bool
  , record :: Bool
  , version :: Bool
  , polybar :: Bool
  , xmobar :: Bool
  }

pomodoroBar :: IO ()
pomodoroBar = ensureRecordExist >> execParser opts >>= parseArgs
  where
    opts = info (myArguments <**> helper)
      ( fullDesc
     <> progDesc "Start a Pomodoro Timer with optional integration with xmobar and polybar"
     <> header "pomodoro-bar - A pausable and configurable Pomodoro Timer with stats for X Window System" )

parseArgs :: Argument -> IO ()
parseArgs (Argument w b l False False False False False) = startTimerManager '-' w b l
parseArgs (Argument w b l False False False True False) = startTimerManager 'p' w b l
parseArgs (Argument w b l False False False False True) = startTimerManager 'm' w b l
parseArgs (Argument w b l True False False False False) = showRecordRaw
parseArgs (Argument w b l False True False False False) = showRecordLast4Weeks w
parseArgs (Argument w b l False False True False False) = showVersion
parseArgs _ = return ()

myArguments :: Parser Argument
myArguments = Argument
  <$> option auto
      ( long "work"
      <> short 'w'
      <> help "work session length in minutes"
      <> showDefault
      <> value 25
      <> metavar "INT")
  <*> option auto
      ( long "break"
      <> short 'b'
      <> help "break session length in minutes"
      <> showDefault
      <> value 5
      <> metavar "INT")
  <*> option auto
      ( long "longbreak"
      <> short 'l'
      <> help "long break session length in minutes"
      <> showDefault
      <> value 15
      <> metavar "INT")
  <*> switch
      ( long "raw"
      <> help "show raw record (in minutes)")
  <*> switch
      ( long "record"
      <> short 'r'
      <> help "show record with 25 minutes per session (adjust using -w option)")
  <*> switch
      ( long "version"
      <> short 'v'
      <> help "show version")
  <*> switch
      ( long "polybar"
      <> help "also update polybar (require additional settings)")
  <*> switch
      ( long "xmobar"
      <> help "also update xmobar (require additional settings)")

showVersion :: IO ()
showVersion = putStrLn
  "pomodoro-bar, version 0.1.0\n\
  \\n\
  \License (SPDX): GPL-2.0-only\n\
  \Author: Theerawat Kiatdarakun"