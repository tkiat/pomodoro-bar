module Common where

getHHMMSS :: Int -> String
getHHMMSS sec =
  let m' = quot sec 60
      h' = quot m' 60
      h
        | h' == 0 = ""
        | otherwise = getTwoDigits h' ++ ":"
      m = getTwoDigits (m' `rem` 60)
      s = getTwoDigits (sec `rem` 60)
   in h ++ m ++ ":" ++ s
  where
    getTwoDigits :: Int -> String
    getTwoDigits x
      | x < 10 = "0" ++ show x
      | otherwise = show x

loopUntilGetChars :: [Char] -> IO Char
loopUntilGetChars charSet = do
  ch <- getChar
  if ch `elem` charSet
    then return ch
    else loopUntilGetChars charSet
