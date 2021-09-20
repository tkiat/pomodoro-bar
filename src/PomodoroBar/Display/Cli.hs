module PomodoroBar.Display.Cli
  ( getUserChoice,
    restoreBufferingAndEcho,
    setNoBufferingAndNoEcho,
    showVersion,
    flushTerm,
    updateCli,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import System.Console.ANSI (clearLine, getTerminalSize, restoreCursor, saveCursor)
import System.IO (BufferMode (NoBuffering), hFlush, hGetBuffering, hGetEcho, hSetBuffering, hSetEcho, stdin, stdout)
import System.Posix.Signals (Handler (Ignore), installHandler, sigINT)
import System.Posix.Terminal (QueueSelector (InputQueue), discardData, queryTerminal)
import System.Posix.Types (Fd)

-- discard all inputs whose file descriptor is associated with a terminal
flushTerm :: Fd -> IO ()
flushTerm fd = queryTerminal fd >>= \b -> when b $ discardData fd InputQueue

getUserChoice :: [Char] -> IO Char
getUserChoice s = bracket (setSigInt Ignore) setSigInt (\_ -> keepAsking s)
  where
    setSigInt :: Handler -> IO Handler
    setSigInt handler = installHandler sigINT handler Nothing

keepAsking :: [Char] -> IO Char
keepAsking s = getChar >>= \c -> if c `elem` s then return c else keepAsking s

restoreBufferingAndEcho :: (BufferMode, Bool) -> IO ()
restoreBufferingAndEcho (b, e) = hSetBuffering stdin b >> hSetEcho stdin e

setNoBufferingAndNoEcho :: IO (BufferMode, Bool)
setNoBufferingAndNoEcho = do
  b <- hGetBuffering stdin
  e <- hGetEcho stdin

  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  return (b, e)

showOneLine :: String -> IO ()
showOneLine t =
  getTerminalSize >>= \case
    Just (_, w) -> putStr $ take w t
    Nothing -> putStr t

showVersion :: IO ()
showVersion =
  putStrLn
    "pomodoro-bar, version 0.1.0\n\
    \\n\
    \License (SPDX): GPL-2.0-only\n\
    \Author: Theerawat Kiatdarakun"

updateCli :: String -> IO ()
updateCli t =
  clearLine >> saveCursor >> showOneLine t >> hFlush stdout >> restoreCursor
