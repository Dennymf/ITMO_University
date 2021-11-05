module HW1.T6
  (
    mcat
  , epart
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat xs = case mconcat xs of
  Just a  -> a
  Nothing -> mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart xs = foldl add (mempty, mempty) xs where
  add (lacc, racc) (Left v)  = (lacc `mappend` v, racc)
  add (lacc, racc) (Right v) = (lacc, racc `mappend` v)