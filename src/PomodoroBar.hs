module PomodoroBar where

import Options.Applicative
import System.Console.ANSI (clearLine)

import Record.Record (ensureRecordExist, showRecordRaw, showRecordLast4Weeks)
import Timer.TimerManager (startTimerManager)

data Argument = Argument
  { work :: Int
  , break :: Int
  , longbreak :: Int
  , cw :: String
  , cb :: String
  , polybar :: Bool
  , xmobar :: Bool
  , raw :: Bool
  , record :: Bool
  , version :: Bool
  }

pomodoroBar :: IO ()
pomodoroBar = ensureRecordExist >> execParser opts >>= parseArgs >> clearLine
  where
    opts = info (myArguments <**> helper)
      ( fullDesc
     <> progDesc "Start a Pomodoro Timer with optional integration with xmobar and polybar. The record file is at $XDG_DATA_HOME/pomodoro-bar/record.json"
     <> header "pomodoro-bar - A pausable and configurable Pomodoro Timer with stats" )

parseArgs :: Argument -> IO ()
parseArgs (Argument _ _ _ _  _ _ _  True _ _) = showRecordRaw
parseArgs (Argument w _ _ _  _ _ _  False True _) = showRecordLast4Weeks w
parseArgs (Argument _ _ _ _  _ _ _  False False True) = showVersion
parseArgs (Argument w b l cw cb False False False False False) = startTimerManager w b l '-' cw cb
parseArgs (Argument w b l cw cb True False False False False) = startTimerManager w b l 'p' cw cb
parseArgs (Argument w b l cw cb False True False False False) = startTimerManager w b l 'm' cw cb
parseArgs _ = return ()

myArguments :: Parser Argument
myArguments = Argument
  <$> option auto
      ( long "work"
      <> short 'w'
      <> help "work duration in minutes"
      <> showDefault
      <> value 25
      <> metavar "INT")
  <*> option auto
      ( long "break"
      <> short 'b'
      <> help "break duration in minutes"
      <> showDefault
      <> value 5
      <> metavar "INT")
  <*> option auto
      ( long "longbreak"
      <> short 'l'
      <> help "long break duration in minutes"
      <> showDefault
      <> value 15
      <> metavar "INT")
  <*> strOption
      ( long "cw"
      <> metavar "COMMAND"
      <> showDefault
      <> value ""
      <> help "System command to execute when work session ends (e.g. \"xset dpms force off\")" )
  <*> strOption
      ( long "cb"
      <> metavar "COMMAND"
      <> showDefault
      <> value ""
      <> help "System command to execute when break session ends if not skip" )
  <*> switch
      ( long "polybar"
      <> help "also update polybar (require additional settings)")
  <*> switch
      ( long "xmobar"
      <> help "also update xmobar (require additional settings)")
  <*> switch
      ( long "raw"
      <> help "show raw record in minutes")
  <*> switch
      ( long "record"
      <> short 'r'
      <> help "show record during the last 4 weeks (adjust using -w option)")
  <*> switch
      ( long "version"
      <> short 'v'
      <> help "show version")

showVersion :: IO ()
showVersion = putStrLn
  "pomodoro-bar, version 0.1.0\n\
  \\n\
  \License (SPDX): GPL-2.0-only\n\
  \Author: Theerawat Kiatdarakun"
