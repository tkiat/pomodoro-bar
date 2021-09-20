{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PomodoroBar.Time.Week
  ( Week (..),
    addSessionToWeek,
    mkWeek,
    mkExistingWeekSummary,
  )
where

import Data.Aeson (FromJSON, KeyValue, ToJSON, object, pairs, parseJSON, toEncoding, toJSON, withObject, (.:), (.=))
import Data.Foldable (toList)
import Data.Sequence (adjust', fromList)
import Data.Text (Text)
import PomodoroBar.Time.Day (DayNum, unDayNum)
import PomodoroBar.Time.Second (Minute (Minute), unMinute)

data Week = Week {mon, tue, wed, thu, fri, sat, sun :: Minute}
  deriving (Eq)

instance FromJSON Week where
  parseJSON = withObject "Week" $ \w -> do
    list <- mapM (fmap Minute . (w .:)) getRecordWeekKeys
    case mkWeek list of Left e -> error e; Right wk -> pure wk

instance ToJSON Week where
  toJSON = object . encodeKv
  toEncoding = pairs . foldl1 (<>) . encodeKv

addSessionToWeek :: Week -> Minute -> DayNum -> Week
addSessionToWeek w m (unDayNum -> d) = do
  let list = toList $ adjust' (+ m) (d - 1) (fromList $ getWeekValues w)
  case mkWeek list of Left e -> error e; Right wk -> wk

encodeKv :: KeyValue kv => Week -> [kv]
encodeKv = zipWith (\k v -> k .= unMinute v) getRecordWeekKeys . getWeekValues

getRecordWeekKeys :: [Text]
getRecordWeekKeys = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

getWeekValues :: Week -> [Minute]
getWeekValues Week {..} = [mon, tue, wed, thu, fri, sat, sun]

maybeDiv :: Double -> Double -> Maybe Double
maybeDiv _ 0 = Nothing
maybeDiv a b = Just $ fromInteger (round (a / b * 10)) / 10.0

mkExistingWeekSummary :: DayNum -> Minute -> Week -> [String]
mkExistingWeekSummary (unDayNum -> d) (Minute w) wk = workload ++ [avg]
  where
    weekRecord = take d $ map (fromIntegral . unMinute) (getWeekValues wk)

    maybeWorkload = mapM (`maybeDiv` fromIntegral w) weekRecord
    workload' = maybe (repeat "N/A") (map show) maybeWorkload
    workload = take 7 $ workload' ++ repeat ""

    maybeAvg = maybeWorkload >>= (`maybeDiv` fromIntegral d) . sum
    avg = maybe "N/A" show maybeAvg

mkWeek :: [Minute] -> Either String Week
mkWeek [mon, tue, wed, thu, fri, sat, sun] = Right Week {..}
mkWeek a = Left $ "mkWeek: invalid argument " ++ show a
