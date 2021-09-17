module TimerDisplay.Bar where

import Control.Monad (when)
import Data.Bits ((.|.))
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode ( ExitFailure ), exitWith)
import System.Posix.Files (createNamedPipe, getFileStatus, isNamedPipe, namedPipeMode, unionFileModes)
import System.Posix.Types (FileMode)

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

fixNamedPipe :: String -> Char -> IO ()
fixNamedPipe path status = case status of
  '-' -> do
    createNamedPipe path fileModeMyNamedPipe
    putStrLn ("Created a new named pipe at " ++ path)
  'x' -> do
    removeFile path
    createNamedPipe path fileModeMyNamedPipe
    putStrLn ("Convert an existing file at  " ++ path ++ " into a named pipe")
  _ -> pure ()
  where
    fileModeMyNamedPipe :: FileMode
    fileModeMyNamedPipe = unionFileModes fileMode644 namedPipeMode

    fileMode644 :: FileMode
    fileMode644 = 256 .|. 128 .|. 32 .|. 4

getNamedPipeStatus :: FilePath -> IO Char
getNamedPipeStatus path = do
  fileExist <- doesFileExist path
  if fileExist
    then do
      status <- getFileStatus path
      if isNamedPipe status
        then return 'o' -- OK
        else return 'x' -- not named pipe
    else do
      return '-' -- file not found

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
