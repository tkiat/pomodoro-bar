{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Record.Record where

import Data.Aeson (defaultOptions, eitherDecode, encode)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON, fieldLabelModifier)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory ( XdgData ))
import GHC.Generics (Generic)

import Record.Date (nthMondayUntilNowLocal, offsetFromThisMonday)

type Record = Map.Map String Week

data Week = Week {
  _Mon :: Int,
  _Tue :: Int,
  _Wed :: Int,
  _Thu :: Int,
  _Fri :: Int,
  _Sat :: Int,
  _Sun :: Int
} deriving (Generic, Show)

$(deriveFromJSON defaultOptions {
  fieldLabelModifier = drop 1
} ''Week)

$(deriveToJSON defaultOptions {
  fieldLabelModifier = drop 1
} ''Week)

recordDir :: IO FilePath
recordDir = getXdgDirectory XdgData "pomodoro-bar"

recordPath :: IO FilePath
recordPath = (++) <$> recordDir <*> pure "/record.json"

ensureRecordExist :: IO ()
ensureRecordExist = do
  d <- recordDir
  createDirectoryIfMissing True d

  fileExist <- recordPath >>= doesFileExist
  if fileExist
    then pure ()
    else do
      path <- recordPath
      writeFile path "{}"

getDecodedRecord :: IO (Either String Record)
getDecodedRecord = eitherDecode <$> getRecord
  where
    getRecord :: IO B.ByteString
    getRecord = recordPath >>= B.readFile

addNewSessionToRecord :: Int -> IO ()
addNewSessionToRecord w = do
  d <- getDecodedRecord
  case d of
    Left err -> do
      path <- recordPath
      putStr $ path ++ " is not a valid JSON\n" ++ err
    Right rec -> do
      mon <- nthMondayUntilNowLocal 1
      o <- offsetFromThisMonday
      let newWeek = case Map.lookup mon rec of
            Just wk -> addSessionToWeek wk o w
            Nothing -> addSessionToWeek emptyWeek o w
              where emptyWeek = Week {_Mon=0,_Tue=0,_Wed=0,_Thu=0,_Fri=0,_Sat=0,_Sun=0}
      let newRec = Map.insert mon newWeek rec
      recordPath >>= (`B.writeFile` encode newRec)
  where
    addSessionToWeek :: Week -> Int -> Int -> Week
    addSessionToWeek wk offset min =
      case offset of
        0 -> wk {_Mon = _Mon wk + min}
        1 -> wk {_Tue = _Tue wk + min}
        2 -> wk {_Wed = _Wed wk + min}
        3 -> wk {_Thu = _Thu wk + min}
        4 -> wk {_Fri = _Fri wk + min}
        5 -> wk {_Sat = _Sat wk + min}
        _ -> wk {_Sun = _Sun wk + min}

showRecordLast4Weeks :: Int -> IO ()
showRecordLast4Weeks workSessionMin = do
  d <- getDecodedRecord
  case d of
    Left err -> do
      path <- recordPath
      putStr $ path ++ " is not a valid JSON\n" ++ err
    Right rec -> do
      putStr $ "Number of Sessions, " ++ show workSessionMin ++ " Minutes Each (Mon - Sun)"
      putStr "\nThis Week      : "
      searchAndShowWeek rec 1
      putStr "\nLast Week      : "
      searchAndShowWeek rec 2
      putStr "\nTwo Weeks Ago  : "
      searchAndShowWeek rec 3
      putStr "\nThree Weeks Ago: "
      searchAndShowWeek rec 4
  where
    searchAndShowWeek :: Record -> Integer -> IO ()
    searchAndShowWeek rec n = do
      mon <- nthMondayUntilNowLocal n
      case Map.lookup mon rec of
        Just wk ->
          case n of
            1 -> offsetFromThisMonday >>= showWeek wk workSessionMin . (+) 1
            _ -> showWeek wk workSessionMin 7
        Nothing -> putStr "--- No Entry ---"
      where
        divideToOneDecimalDigit :: Int -> Int -> Double
        divideToOneDecimalDigit a b = fromInteger ((round :: Double -> Integer) (fromIntegral a/fromIntegral b * 10)) / 10.0

        showWeek :: Week -> Int -> Int -> IO ()
        showWeek wk w n =
          let
            weekWorkloadFull = [_Mon wk , _Tue wk , _Wed wk , _Thu wk , _Fri wk , _Sat wk , _Sun wk]
            weekWorkload = take n weekWorkloadFull
            sessionWeek = [show $ divideToOneDecimalDigit x w| x <- weekWorkload]
            avgPerDay = divideToOneDecimalDigit (sum weekWorkload) (w * length weekWorkload)
          in
            putStr $ "[" ++ intercalate ", " sessionWeek ++ "] Avg: " ++ show avgPerDay

showRecordRaw :: IO ()
showRecordRaw = recordPath >>= readFile >>= putStr