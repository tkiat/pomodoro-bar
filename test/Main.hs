{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
-- import PomodoroBar

prop_test :: Property
prop_test = property $ do
  "TODO" === "TODO"
--   doPomodoro === "Pomodoro"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
