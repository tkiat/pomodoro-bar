-- TODO press s repeatly not good
-- TODO stuck at getchar both sigINT handler and CTRL+Z
-- TODO prevent store float in record, also changes this for python

module PomodoroBar where

import Options.Applicative
import System.Console.ANSI (clearLine)

import Record.Record (ensureRecordExist, showRecordRaw, showRecordLast4Weeks)
import Timer.TimerManager (startTimerManager)

data Argument = Argument
  { work :: Int
  , break :: Int
  , longbreak :: Int
  , cmdwork :: String
  , cmdbreak :: String
  , raw :: Bool
  , record :: Bool
  , version :: Bool
  , polybar :: Bool
  , xmobar :: Bool
  }

pomodoroBar :: IO ()
pomodoroBar = ensureRecordExist >> execParser opts >>= parseArgs >> clearLine
  where
    opts = info (myArguments <**> helper)
      ( fullDesc
     <> progDesc "Start a Pomodoro Timer with optional integration with xmobar and polybar"
     <> header "pomodoro-bar - A pausable and configurable Pomodoro Timer with stats for X Window System" )

parseArgs :: Argument -> IO ()
parseArgs (Argument _ _ _ _  _  True _ _ _ _) = showRecordRaw
parseArgs (Argument w _ _ _  _  False True _ _ _) = showRecordLast4Weeks w
parseArgs (Argument _ _ _ _  _  False False True _ _) = showVersion
parseArgs (Argument w b l cw cb False False False False False) = startTimerManager w b l '-' cw cb
parseArgs (Argument w b l cw cb False False False True False) = startTimerManager w b l 'p' cw cb
parseArgs (Argument w b l cw cb False False False False True) = startTimerManager w b l 'm' cw cb
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
  <*> strOption
      ( long "cmdwork"
      <> metavar "COMMAND"
      <> help "System command to execute when work session ends (e.g. \"xset dpms force off\")" )
  <*> strOption
      ( long "cmdbreak"
      <> metavar "COMMAND"
      <> help "System command to execute when break session ends (without skipping)" )
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
