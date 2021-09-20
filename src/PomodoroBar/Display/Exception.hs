module PomodoroBar.Display.Exception
  ( MyException (..),
    handleMyException,
  )
where

import Control.Exception (Exception)
import System.Exit (exitFailure)

data MyException
  = CannotParseRecord
  | NamedPipeNotValid
  | NotAnExecutable String
  deriving (Show)

instance Exception MyException

handleMyException :: MyException -> IO b
handleMyException e = putStrLn ("*** Exception: " ++ show e) >> exitFailure
