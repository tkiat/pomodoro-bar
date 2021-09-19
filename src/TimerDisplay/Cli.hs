module TimerDisplay.Cli where

import System.Console.ANSI (clearLine, restoreCursor, saveCursor)
import System.IO (hFlush, stdout)

getKeysHint :: Char -> String
getKeysHint timerSessionCode = case timerSessionCode of
  'w' -> "CTRL+c to Pause"
  'i' -> "[s]tart or [q]uit"
  _ -> "CTRL+c to Skip"

getProgressBar :: Int -> String
getProgressBar sessionNumRem =
  let original = "w-b-w-b-w-b-w-l"
      indexLeft = 2 * (sessionNumRem - 1)
      indexRight
        | sessionNumRem /= 8 = indexLeft
        | otherwise = indexLeft + 1
      (p1, p2and3) = splitAt indexLeft original
      (p2, p3) = splitAt (indexRight - indexLeft + 1) p2and3
   in p1 ++ "[" ++ p2 ++ "]" ++ p3

updateCli :: String -> IO ()
updateCli text = do
  clearLine
  saveCursor
  putStr $ text ++ " "
  hFlush stdout
  restoreCursor
