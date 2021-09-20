{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PomodoroBar.Time.Second
  ( Minute (..),
    Second (..),
    toMin,
    toSecond,
  )
where

newtype Minute = Minute {unMinute :: Int}
  deriving (Eq, Read)
  deriving newtype (Num)

instance Show Minute where
  show (Minute m) = show m

newtype Second = Second Int
  deriving (Eq, Ord, Show)
  deriving newtype (Num)

toMin :: Second -> Minute
toMin (Second s) = Minute (quot s 60)

toSecond :: Minute -> Second
toSecond (Minute m) = Second (m * 60)
