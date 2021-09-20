{-# LANGUAGE DeriveGeneric #-}

module PomodoroBar.Record.Internal
  ( Record (..),
    mkUpdatedRecord,
    ensureRecordExist,
    mkWeekSummary,
    prettifyRecordSummary,
    prettifyRecordSummaryOneRow,
    recordPath,
    showRecordRaw,
    showRecordSummary,
    updateRecord,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad (liftM2, unless)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict, defaultOptions, encodeFile, genericToEncoding, toEncoding)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import PomodoroBar.Display.Exception (MyException (CannotParseRecord), handleMyException)
import PomodoroBar.Time (DayNum, Minute (..), Week (..), addSessionToWeek, getDayNumber, getNthMondayUntilNow, mkDayNum, mkExistingWeekSummary)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

newtype Record = Record (Map.Map String Week) deriving (Generic)

instance FromJSON Record

-- toEncoding has no intermediate value unlike toJSON
instance ToJSON Record where toEncoding = genericToEncoding defaultOptions

decodeRecord :: IO Record
decodeRecord =
  recordPath >>= decodeFileStrict >>= \case
    Just r -> pure r
    Nothing -> throwIO CannotParseRecord `catch` handleMyException

ensureRecordExist :: IO ()
ensureRecordExist = ensureRecordFolderExist >> ensureRecordFileExist
  where
    ensureRecordFileExist, ensureRecordFolderExist :: IO ()
    ensureRecordFileExist =
      recordPath >>= \r ->
        doesFileExist r >>= \b -> unless b $ writeFile r "{}"
    ensureRecordFolderExist = recordDir >>= createDirectoryIfMissing True

getWeekInRecord :: Record -> Int -> IO (Maybe Week)
getWeekInRecord (Record m) o = flip Map.lookup m <$> getNthMondayUntilNow o

mkUpdatedRecord :: Record -> String -> DayNum -> Minute -> Record
mkUpdatedRecord (Record m) mo@(flip Map.lookup m -> Just _) d w =
  Record $ Map.adjust (\wk -> addSessionToWeek wk w d) mo m
mkUpdatedRecord (Record m) mo d w =
  Record $ Map.insert mo (addSessionToWeek (Week 0 0 0 0 0 0 0) w d) m

mkWeekSummary :: Minute -> DayNum -> Maybe Week -> [String]
mkWeekSummary w d = maybe (replicate 8 "") (mkExistingWeekSummary d w)

padSpaces :: Int -> String -> String
padSpaces n s = replicate (n - length s) ' ' ++ s

prettifyRecordSummary :: [[String]] -> [String]
prettifyRecordSummary summary =
  let header = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Avg"]
      headerSep = repeat "---"
      res = header : headerSep : summary
      maxCols = foldl (zipWith max) (repeat 0) (map (map length) res)
   in map (prettifyRecordSummaryOneRow maxCols) res

prettifyRecordSummaryOneRow :: [Int] -> [String] -> String
prettifyRecordSummaryOneRow = (intercalate "  " .) . zipWith padSpaces

recordDir, recordPath :: IO FilePath
recordDir = getXdgDirectory XdgData "pomodoro-bar"
recordPath = liftM2 (</>) recordDir (pure "record.json")

showRecordRaw :: IO ()
showRecordRaw = recordPath >>= readFile >>= putStrLn

showRecordSummary :: Minute -> Int -> IO ()
showRecordSummary w numWeek = do
  rec <- decodeRecord
  dayNum <- getDayNumber

  firstWeek <- getWeekInRecord rec 1
  restWeek <- mapM (getWeekInRecord rec) [2 .. numWeek]

  let first = mkWeekSummary w dayNum firstWeek
  let rest = map (mkWeekSummary w (mkDayNum 7)) restWeek
  let summary = first : rest

  putStrLn $ "Number of " ++ show w ++ "-minute sessions from this week (top)"
  mapM_ putStrLn $ prettifyRecordSummary summary

updateRecord :: Minute -> IO ()
updateRecord m = do
  rec <- decodeRecord
  mo <- getNthMondayUntilNow 1
  d <- getDayNumber
  recordPath >>= flip encodeFile (mkUpdatedRecord rec mo d m)
