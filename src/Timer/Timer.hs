module Timer.Timer where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, tryTakeMVar)

import Timer.Bar (updateBar)
import Timer.Cli (updateCli)
import Common (getHHMMSS)

startTimer :: MVar() -> String -> String -> String -> Char -> Char -> Int -> IO Int
startTimer v prefix suffix prefixBar timerSessionCode barType sec = do
  let timerValue = getHHMMSS sec

  updateBar (prefixBar ++ timerValue) timerSessionCode barType
  updateCli $ prefix ++ " " ++ timerValue ++ " - " ++ suffix

  threadDelay (1 * 1000000)
  val <- tryTakeMVar v
  case val of
    Just _ -> case timerSessionCode of
      'w' -> updateBar ("[P]" ++ timerValue) 'i' barType >> return sec
      _ -> return 0
    Nothing -> do
      if sec > 0
        then startTimer v prefix suffix prefixBar timerSessionCode barType (sec - 1)
        else return 0
