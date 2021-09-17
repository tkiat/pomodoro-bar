module Timer.TimerManager where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (when)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import System.Posix.IO (stdInput)
import System.Posix.Signals (installHandler, sigINT)
import System.Posix.Terminal (discardData, QueueSelector( InputQueue ), queryTerminal)
import System.Posix.Types (Fd)
import System.Process (callCommand)

import Common (getHHMMSS, loopUntilGetChars, sigIntInnerHandler)
import Record.Record (addNewSessionToRecord)
import Timer.Bar (ensureNamedPipesExist, updateBar)
import Timer.Cli (getDuration, getKeysHint, getProgressBar, updateCli)
import Timer.Timer (startTimer)

startTimerManager :: Int -> Int -> Int -> Char -> String -> String -> IO ()
startTimerManager w b l barType cmdW cmdB = do
  ensureNamedPipesExist barType

  hSetBuffering stdin NoBuffering
  timerManager 1 w b l 0
  where
    timerManager :: Int -> Int -> Int -> Int -> Int -> IO ()
    timerManager sessionNum w b l sec = do
      let sessionNumRem = ((sessionNum - 1) `rem` 8) + 1
      let timerSessionCode = getTimerSessionCode sessionNumRem
      let duration | sec > 0 = sec
                   | otherwise = getDuration timerSessionCode w b l
      let progressBar = getProgressBar sessionNumRem

      when (sec == 0) (
        if timerSessionCode == 'w'
          then updateBar ("[" ++ show sessionNumRem ++ "]START") 'i' barType
          else updateBar ("[" ++ show sessionNumRem ++ "]BREAK") 'i' barType
        )

      updateCli $ progressBar ++ " " ++ getHHMMSS duration ++ " - " ++ getKeysHint 'i'

      termFlush stdInput
      userChoice <- loopUntilGetChars ['q', 's']
      case userChoice of
        's' -> do
          sub <- newEmptyMVar
          _ <- installHandler sigINT (sigIntInnerHandler sub) Nothing
          let prefixBar = "[" ++ show sessionNumRem ++ "]"
          secRemaining <- startTimer sub progressBar (getKeysHint timerSessionCode) prefixBar timerSessionCode barType duration
          when (secRemaining == 0 && timerSessionCode == 'w') (callCommand cmdW >> addNewSessionToRecord w)
          when (secRemaining == 0 && (timerSessionCode == 'b' || timerSessionCode == 'l')) (callCommand cmdB)
          let newSessionNum | secRemaining > 0 = sessionNum
                            | otherwise = sessionNum + 1
          let newSecRemaining | secRemaining == (-1) = 0 -- (-1) means user skip break
                              | otherwise = secRemaining
          timerManager newSessionNum w b l newSecRemaining
        _ -> do
          updateBar "POMODORO" 'i' barType

    getTimerSessionCode :: Int -> Char
    getTimerSessionCode sessionNumRem
      | sessionNumRem `elem` [1, 3, 5, 7] = 'w'
      | sessionNumRem `elem` [2, 4, 6] = 'b'
      | otherwise = 'l'

-- discard all inputs whose file descriptor is associated with a terminal
termFlush :: Fd -> IO ()
termFlush fd = do
  isTerm <- queryTerminal fd
  when isTerm $ discardData fd InputQueue
