module Timer.NamedPipe where

import Data.Bits ((.|.))
import System.Directory (doesFileExist, removeFile)
import System.Posix.Files (createNamedPipe, getFileStatus, isNamedPipe, namedPipeMode, unionFileModes)
import System.Posix.Types (FileMode)

fileModeMyNamedPipe :: FileMode
fileModeMyNamedPipe = unionFileModes fileMode644 namedPipeMode
  where
    fileMode644 :: FileMode
    fileMode644 = 256 .|. 128 .|. 32 .|. 4

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
