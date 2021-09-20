module PomodoroBar.Display.Bar
  ( BarType (..),
    ensureNamedPipesExist,
    updateBar,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad (unless)
import Data.Bits ((.|.))
import Data.Tuple (swap)
import PomodoroBar.Display.Exception (MyException (NamedPipeNotValid), handleMyException)
import System.Directory (doesFileExist, removeFile)
import System.Posix.Files (createNamedPipe, getFileStatus, isNamedPipe, namedPipeMode, unionFileModes)
import System.Posix.Types (FileMode)

data BarType = None | Polybar | Xmobar deriving (Bounded, Enum, Eq)

instance Show BarType where
  show None = "none"
  show Polybar = "polybar"
  show Xmobar = "xmobar"

data NamedPipeStatus = NotFound | NotNamedPipe | Valid deriving (Eq)

ensureNamedPipesExist :: BarType -> IO ()
ensureNamedPipesExist t@(getNamedPipePaths -> Just (w, i)) = do
  [isOk1, isOk2] <- mapM ensureNamedPipeExist [w, i]
  unless (isOk1 && isOk2) (handleNamedPipeInvalid t)
ensureNamedPipesExist _ = pure ()

ensureNamedPipeExist :: FilePath -> IO Bool
ensureNamedPipeExist p =
  getNamedPipeStatus p >>= \s -> fixNamedPipe p s >> return (s == Valid)

fixNamedPipe :: FilePath -> NamedPipeStatus -> IO ()
fixNamedPipe p = \case
  NotFound ->
    createNamedPipe p fileModeNamedPipe
      >> putStrLn ("Created a new named pipe at " ++ p)
  NotNamedPipe ->
    removeFile p >> createNamedPipe p fileModeNamedPipe
      >> putStrLn ("Replaced an existing file with a named pipe at " ++ p)
  Valid -> pure ()
  where
    fileMode644, fileModeNamedPipe :: FileMode

    fileMode644 = 256 .|. 128 .|. 32 .|. 4
    fileModeNamedPipe = unionFileModes fileMode644 namedPipeMode

getNamedPipePaths :: BarType -> Maybe (String, String)
getNamedPipePaths None = Nothing
getNamedPipePaths _ = Just ("/tmp/.pomodoro-bar-w", "/tmp/.pomodoro-bar-i")

getNamedPipeStatus :: FilePath -> IO NamedPipeStatus
getNamedPipeStatus p =
  doesFileExist p >>= \case
    True -> do
      ok <- isNamedPipe <$> getFileStatus p
      return $ if ok then Valid else NotNamedPipe
    _ -> return NotFound

mkBarTexts :: Bool -> String -> (String, String)
mkBarTexts b t = let i = (t ++ "\n", "\n") in if b then i else swap i

handleNamedPipeInvalid :: BarType -> IO ()
handleNamedPipeInvalid t = do
  case t of
    Xmobar -> putStr "Please recompile xmobar and rerun"
    _ -> putStr "Please rerun"
  throwIO NamedPipeNotValid `catch` handleMyException

updateBar :: BarType -> Bool -> String -> IO ()
updateBar (getNamedPipePaths -> Just (w, i)) isWorkMode t =
  let (wt, it) = mkBarTexts isWorkMode t in writeFile w wt >> writeFile i it
updateBar _ _ _ = pure ()
