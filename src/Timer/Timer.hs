module Timer.Timer where

import Common (getHHMMSS)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, tryTakeMVar)
import TimerDisplay.Bar (updateBar)
import TimerDisplay.Cli (updateCli)

startTimer :: MVar () -> String -> String -> String -> Char -> Char -> Int -> IO Int
startTimer timerMVar prefixCli suffixCli prefixBar timerSessionCode barType sec = do
  let timerValue = getHHMMSS sec

  updateBar (prefixBar ++ timerValue) timerSessionCode barType
  updateCli $ prefixCli ++ " " ++ timerValue ++ " - " ++ suffixCli

  threadDelay (1 * 1000000)
  val <- tryTakeMVar timerMVar
  case val of
    Just _ -> case timerSessionCode of
      'w' -> updateBar ("[P]" ++ timerValue) 'i' barType >> return sec
      _ -> return (-1)
    Nothing -> do
      if sec > 0
        then startTimer timerMVar prefixCli suffixCli prefixBar timerSessionCode barType (sec - 1)
        else return 0
