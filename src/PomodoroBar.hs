{-# LANGUAGE MultiWayIf #-}

module PomodoroBar
  ( parseArgs,
    pomodoroBar,
  )
where

import Control.Exception (bracket, finally)
import Options.Applicative (execParser)
import PomodoroBar.Argument.Option (CliOption (..), mkCliOptions)
import PomodoroBar.Display (BarType, ensureNamedPipesExist, restoreBufferingAndEcho, setNoBufferingAndNoEcho, showVersion)
import PomodoroBar.Record (ensureRecordExist, showRecordRaw, showRecordSummary)
import PomodoroBar.Session (Session, TimerIdleStatus (ToBegin), mkCommand, mkSession, startSession)
import PomodoroBar.Time (Minute, toSecond)
import System.Console.ANSI (clearLine, hideCursor, showCursor)
import System.Posix.Signals (Handler (Ignore), installHandler, sigTSTP)

data UserChoice
  = Pomodoro Minute Minute Minute Int String String BarType
  | RecordRaw
  | RecordSummary Minute Int
  | Version

execute :: UserChoice -> IO ()
execute = \case
  Pomodoro (toSecond -> w) (toSecond -> b) (toSecond -> l) s cw cb bt -> do
    [cw', cb'] <- mapM mkCommand [cw, cb]
    ensureNamedPipesExist bt
    hideCursor
    o <- setNoBufferingAndNoEcho
    let sessions = map (mkSession w b l cw' cb') [(s * 2 - 1) ..]
    bracket (setSigTSTP Ignore) setSigTSTP (\_ -> loop sessions)
      `finally` (clearLine >> restoreBufferingAndEcho o >> showCursor)
    where
      loop :: [Session] -> IO ()
      loop = mapM_ (startSession bt ToBegin 0)

      setSigTSTP :: Handler -> IO Handler
      setSigTSTP handler = installHandler sigTSTP handler Nothing
  RecordRaw -> ensureRecordExist >> showRecordRaw
  RecordSummary w n -> ensureRecordExist >> showRecordSummary w n
  Version -> showVersion

getUserChoice :: CliOption -> UserChoice
getUserChoice
  CliOption
    { oWork = w,
      oBreak = b,
      oLongBreak = l,
      oSessionNum = s,
      oCmdWork = cw,
      oCmdBreak = cb,
      oBarType = bt,
      oRecordRaw = rr,
      oRecordSummary = rs,
      oNumWeek = n,
      oVersion = v
    } =
    if
        | v -> Version
        | rs -> RecordSummary w n
        | rr -> RecordRaw
        | otherwise -> Pomodoro w b l s cw cb bt

parseArgs :: IO UserChoice
parseArgs = getUserChoice <$> (mkCliOptions >>= execParser)

pomodoroBar :: IO ()
pomodoroBar = parseArgs >>= execute
