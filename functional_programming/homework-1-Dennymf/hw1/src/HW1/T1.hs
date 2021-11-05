module HW1.T1
  (
    Day (..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

import GHC.Natural

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq)

nextDay :: Day -> Day
nextDay day =
  case day of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays n day = afterDays (n - 1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend day =
  case day of
    Saturday -> True
    Sunday   -> True
    _        -> False

daysToParty :: Day -> Natural
daysToParty day = daysToParty' day 0 where
  daysToParty' Friday n = n
  daysToParty' day' n    = daysToParty' (nextDay day') (n + 1)