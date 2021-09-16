module Timer.TimerManager where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (when)
import System.Console.ANSI (clearLine, hideCursor, showCursor)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Posix.Signals (installHandler, sigINT)

import Common (getHHMMSS, loopUntilGetChars, sigIntInnerHandler)
import Record.Record (addNewSessionToRecord)
import Timer.Bar (ensureNamedPipesExist, updateBar)
import Timer.Cli (getDuration, getKeysHint, getProgressBar, updateCli)
import Timer.Timer (startTimer)

startTimerManager :: Char -> Int -> Int -> Int -> IO ()
startTimerManager barType w b l = do
  ensureNamedPipesExist barType

  hSetBuffering stdin NoBuffering
  hideCursor
  hSetEcho stdin False
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

      userChoice <- loopUntilGetChars ['q', 's']
      case userChoice of
        's' -> do
          sub <- newEmptyMVar
          _ <- installHandler sigINT (sigIntInnerHandler sub) Nothing
          let prefixBar = "[" ++ show sessionNumRem ++ "]"
          secRemaining <- startTimer sub progressBar (getKeysHint timerSessionCode) prefixBar timerSessionCode barType duration
          when (secRemaining == 0 && timerSessionCode == 'w') (addNewSessionToRecord w)
          let newSessionNum | secRemaining == 0 = sessionNum + 1
                            | otherwise = sessionNum
          timerManager newSessionNum w b l secRemaining
        _ -> do
          clearLine
          showCursor
          hSetEcho stdin True
          updateBar "POMODORO" 'i' barType

    getTimerSessionCode :: Int -> Char
    getTimerSessionCode sessionNumRem
      | sessionNumRem `elem` [1, 3, 5, 7] = 'w'
      | sessionNumRem `elem` [2, 4, 6] = 'b'
      | otherwise = 'l'
