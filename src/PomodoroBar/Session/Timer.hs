module PomodoroBar.Session.Timer
  ( DisplayContext (..),
    mkBarText,
    mkCliText,
    mkTimerDigit,
    startTimer,
    updateDisplay,
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import PomodoroBar.Display (BarType, updateBar, updateCli)
import PomodoroBar.Time (Second (..))
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT)
import Text.Printf (printf)

data DisplayContext = DisplayContext
  { barIsWorkMode :: Bool,
    barLabel :: String,
    barStatusText :: String,
    barType :: BarType,
    keyHint :: String,
    progressBar :: String,
    second :: Second
  }

mkBarText :: DisplayContext -> String -> String
mkBarText c@(null . barStatusText -> True) digit = barLabel c ++ digit
mkBarText c _ = barLabel c ++ barStatusText c

mkCliText :: DisplayContext -> String -> String
mkCliText c digit = progressBar c ++ " " ++ digit ++ " - " ++ keyHint c

mkTimerDigit :: Second -> String
mkTimerDigit (Second sec) = h' ++ m ++ ":" ++ s
  where
    h' = let h = quot sec 3600 in if h > 0 then printf "%02d" h ++ ":" else ""
    (s, m) = join bimap (printf "%02d" . (`mod` 60)) (sec, quot sec 60)

startTimer :: DisplayContext -> IO Second
startTimer c = do
  v <- newEmptyMVar
  _ <- installHandler sigINT (CatchOnce $ putMVar v ()) Nothing
  t <- newIORef (second c)
  bracket (forkIO $ timer v t c) killThread (\_ -> takeMVar v)
  readIORef t

timer :: MVar () -> IORef Second -> DisplayContext -> IO ()
timer v t c = go (second c)
  where
    go, tick :: Second -> IO ()

    go s@((> 0) -> True) = tick s >> go (s - 1)
    go _ = putMVar v ()

    tick s = do
      updateDisplay c {second = s}
      wait $ Second 1
      modifyIORef' t (subtract 1)

updateDisplay :: DisplayContext -> IO ()
updateDisplay c =
  let digit = mkTimerDigit $ second c
      (bt, ct) = (mkBarText c digit, mkCliText c digit)
   in updateBar (barType c) (barIsWorkMode c) bt >> updateCli ct

wait :: Second -> IO ()
wait (Second s) = threadDelay $ s * 1000000
