module PomodoroBar.Time.Day
  ( DayNum,
    getDayNumber,
    mkDayNum,
    getNthMondayUntilNow,
    unDayNum,
  )
where

import Data.Ix (inRange)
import Data.Time.Calendar (Day, addDays)
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)

newtype DayNum = DayNum {unDayNum :: Int}

getDayNumber :: IO DayNum
getDayNumber = mkDayNum . snd . mondayStartWeek <$> getTodayLocal

getNthMondayUntilNow :: Int -> IO String
getNthMondayUntilNow n = do
  (DayNum d) <- getDayNumber
  let day = addDays (- (toInteger ((d - 1) + 7 * (n - 1)))) <$> getTodayLocal
  show <$> day

getTodayLocal :: IO Day
getTodayLocal = localDay <$> (zonedTimeToLocalTime <$> getZonedTime)

mkDayNum :: Int -> DayNum
mkDayNum a@(inRange (1, 7) -> True) = DayNum a
mkDayNum _ = error "Day number must be in range [1, 7]"
