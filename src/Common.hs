module Common where

import Control.Concurrent.MVar (MVar, putMVar)
import System.Posix.Signals (Handler (CatchOnce))

getHHMMSS :: Int -> String
getHHMMSS sec =
  let
    h' = quot sec 3600
    h | h' == 0 = ""
      | otherwise = getTwoDigits h' ++ ":"
    m = getTwoDigits (quot sec 60)
    s = getTwoDigits (sec `rem` 60)
  in
    h ++ m ++ ":" ++ s
  where
    getTwoDigits :: Int -> String
    getTwoDigits x | x < 10 = "0" ++ show x
                   | otherwise = show x

loopUntilGetChars :: [Char] -> IO Char
loopUntilGetChars charSet = do
  ch <- getChar
  if ch `elem` charSet
    then return ch
    else loopUntilGetChars charSet

sigIntInnerHandler :: MVar () -> Handler
sigIntInnerHandler v = CatchOnce $ do
  putMVar v ()
