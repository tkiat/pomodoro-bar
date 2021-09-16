module Timer.Bar where

import Control.Monad (when)
import System.Exit (ExitCode ( ExitFailure ), exitWith)
import Timer.NamedPipe (fixNamedPipe, getNamedPipeStatus)

data PipePaths = PipePaths { work :: FilePath, idle :: FilePath }

ensureNamedPipesExist :: Char -> IO ()
ensureNamedPipesExist barType = do
  case getPipePaths barType of
    Just paths -> do
      let pathW = work paths
      let pathI = idle paths
      statusW <- getNamedPipeStatus pathW
      statusI <- getNamedPipeStatus pathI
      when (statusW /= 'o' || statusI /= 'o') (
        fixNamedPipe pathW statusW >>
        fixNamedPipe pathI statusI >>
        putStr "Please" >>
        when (barType == 'm') (putStr " recompile xmobar and") >>
        putStrLn " rerun" >>
        exitWith (ExitFailure 1))
    Nothing -> pure ()

getPipePaths :: Char -> Maybe PipePaths
getPipePaths barType
  | barType `elem` ['m', 'p'] = Just PipePaths { work = "/tmp/.pomodoro-bar-w", idle = "/tmp/.pomodoro-bar-i" }
  | otherwise = Nothing

updateBar :: String -> Char -> Char -> IO ()
updateBar text sessionCode barType = do
  case getPipePaths barType of
    Just paths -> case sessionCode of
      'w' -> writeFile (idle paths) "\n" >> writeFile (work paths) (text ++ "\n")
      _ -> writeFile (idle paths) (text ++ "\n") >> writeFile (work paths) "\n"
    Nothing -> pure ()
