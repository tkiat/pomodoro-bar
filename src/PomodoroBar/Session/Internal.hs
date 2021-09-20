module PomodoroBar.Session.Internal
  ( Command (..),
    Session (..),
    SessionType (..),
    TimerIdleStatus (ToBegin),
    mkProgressBar,
    mkSession,
    mkCommand,
    startSession,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad (unless, when)
import Data.Maybe (isJust, listToMaybe)
import PomodoroBar.Display (BarType, MyException (NotAnExecutable), flushTerm, getUserChoice, handleMyException, updateBar)
import PomodoroBar.Record (ensureRecordExist, updateRecord)
import PomodoroBar.Session.Timer (DisplayContext (..), startTimer, updateDisplay)
import PomodoroBar.Time (Second (..), toMin)
import System.Directory (findExecutable)
import System.Exit (exitSuccess)
import System.Posix.IO (stdInput)
import System.Process (callCommand)

newtype Command = Command {unCommand :: String}

data Session = Session
  {sCommand :: Command, sNum :: Int, sSeconds :: Second, sType :: SessionType}

data SessionType = Work | Rest deriving (Eq, Ord, Show)

data TimerIdleStatus = Pause | ToBegin deriving (Eq)

handleTimerEnd :: Session -> Second -> IO ()
handleTimerEnd s@(sType -> Work) 0 =
  ensureRecordExist >> updateRecord (toMin $ sSeconds s)
    >> callCommand (unCommand $ sCommand s)
handleTimerEnd s 0 = callCommand (unCommand $ sCommand s)
handleTimerEnd _ _ = pure ()

mkBarStatus :: TimerIdleStatus -> Session -> String
mkBarStatus Pause _ = "PAUSE"
mkBarStatus _ (sType -> Work) = "START"
mkBarStatus _ _ = "BREAK"

mkCommand :: String -> IO Command
mkCommand s = checkCommand s >> return (Command s)
  where
    checkCommand :: String -> IO ()
    checkCommand c = mightThrowIO c `catch` handleMyException

    mightThrowIO :: String -> IO ()
    mightThrowIO (listToMaybe . words -> Just baseCmd) =
      findExecutable baseCmd >>= \b ->
        unless (isJust b) (throwIO $ NotAnExecutable (baseCmd ++ " not found"))
    mightThrowIO _ = pure ()

mkProgressBar :: Session -> String
mkProgressBar s =
  let original = "w-b-w-b-w-b-w-l"
      i = 4 * ((sNum s - 1) `mod` 4) + (if sType s == Work then 0 else 2)
      (p1, rest) = splitAt i original
   in p1 ++ "[" ++ take 1 rest ++ "]" ++ drop 1 rest

mkSession :: Second -> Second -> Second -> Command -> Command -> Int -> Session
mkSession w b l cmdW cmdB n =
  Session
    { sNum = floor (fromIntegral (n + 1) / 2 :: Double) :: Int,
      sCommand = chooseAlternate cmdW cmdB n,
      sSeconds = chooseAlternateExceptMult8 w b l n,
      sType = chooseAlternate Work Rest n
    }
  where
    chooseAlternate :: a -> a -> Int -> a
    chooseAlternate c1 c2 i = [c1, c2] !! ((i - 1) `mod` 2)

    chooseAlternateExceptMult8 :: a -> a -> a -> Int -> a
    chooseAlternateExceptMult8 _ _ c3 ((`mod` 8) -> 0) = c3
    chooseAlternateExceptMult8 c1 c2 _ i = chooseAlternate c1 c2 i

startSession :: BarType -> TimerIdleStatus -> Second -> Session -> IO ()
startSession bt ts sec s = do
  updateDisplay ctx
  flushTerm stdInput >> getUserChoice ['q', 's'] >>= \case
    's' -> do
      sec' <- startTimer timerCtx
      handleTimerEnd s sec'
      when (willRepeatSession s sec') $ startSession (barType ctx) Pause sec' s
    _ -> updateBar (barType ctx) (barIsWorkMode ctx) "POMODORO" >> exitSuccess
  where
    ctx =
      DisplayContext
        { barLabel = "[" ++ show (sNum s) ++ "]",
          barIsWorkMode = False,
          barStatusText = mkBarStatus ts s,
          barType = bt,
          keyHint = "[s]tart or [q]uit",
          progressBar = mkProgressBar s,
          second = if ts == Pause then sec else sSeconds s
        }
    timerCtx =
      let w = sType s == Work
       in ctx
            { barStatusText = "",
              barIsWorkMode = w,
              keyHint = if w then "CTRL+c to Pause" else "CTRL+c to Skip"
            }

willRepeatSession :: Session -> Second -> Bool
willRepeatSession s sec = (sec /= 0) && (sType s == Work)
