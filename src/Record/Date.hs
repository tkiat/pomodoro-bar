module Record.Date where

import Data.Time.Clock
import Data.Time.Calendar (Day, addDays)
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import Data.Time.LocalTime (getZonedTime, zonedTimeToUTC)

todayLocal :: IO Day
todayLocal = utctDay <$> (zonedTimeToUTC <$> getZonedTime)

offsetFromThisMonday :: IO Int
offsetFromThisMonday = todayLocal >>= pure . (+) (-1) . snd . mondayStartWeek

nthMondayUntilNowLocal' :: Integer -> IO Day
nthMondayUntilNowLocal' n = do
  o <- (*) (-1) . toInteger <$> offsetFromThisMonday
  addDays (o - 7 * (n - 1)) <$> todayLocal

nthMondayUntilNowLocal :: Integer -> IO String
nthMondayUntilNowLocal n = show <$> nthMondayUntilNowLocal' n
