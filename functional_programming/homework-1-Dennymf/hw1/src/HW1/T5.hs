module HW1.T5
  (
    splitOn
  , joinWith
  ) where

import Data.List.NonEmpty as NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep xs = foldr split ([] :| []) xs where
  split x acc@(h :| t)
    | x == sep = [] <| acc
    | otherwise = (x : h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep xs = foldl1 join xs where
  join acc x = acc ++ [sep] ++ x