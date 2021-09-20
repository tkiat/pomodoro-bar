module PomodoroBar.Argument.Parser
  ( parserBartype,
    parserBool,
    parserMinute,
    parserPosInt,
    parserString,
  )
where

import Data.Maybe (maybeToList)
import Options.Applicative (FlagFields, Mod, OptionFields, Parser, ReadM, option, readerError, str, strOption, switch)
import PomodoroBar.Display (BarType (..))
import PomodoroBar.Time (Minute (..))
import Text.Read (readMaybe)

parserBartype :: Mod OptionFields BarType -> Parser BarType
parserBartype = option readerBarType

parserBool :: Mod FlagFields Bool -> Parser Bool
parserBool = switch

parserMinute :: Mod OptionFields Minute -> Parser Minute
parserMinute = option (fmap Minute readerPosInt)

parserPosInt :: Mod OptionFields Int -> Parser Int
parserPosInt = option readerPosInt

parserString :: Mod OptionFields String -> Parser String
parserString = strOption

readerBarType :: ReadM BarType
readerBarType =
  str >>= \case
    "polybar" -> return Polybar
    "xmobar" -> return Xmobar
    "none" -> return None
    _ -> readerError $ "choose from " ++ show [(minBound :: BarType) ..]

readerPosInt :: ReadM Int
readerPosInt =
  str >>= \case
    (sum . maybeToList . readMaybe -> n) | n > 0 -> pure n
    _ -> readerError "must be a positive integer"
