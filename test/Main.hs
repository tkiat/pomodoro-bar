-- usage: cabal test
module Main where

import Module.PomodoroBar (testUserChoice)
import Module.Record (testRecord)
import Module.Session (testSession)
import Module.Time.Day (testTimeDay)
import Module.Time.Second (testTimeSecond)
import Module.Time.Week (testTimeWeek)
import Module.Timer (testTimer)

main :: IO ()
main = do
  testRecord
  testSession
  testTimeDay
  testTimeSecond
  testTimeWeek
  testTimer
  testUserChoice
