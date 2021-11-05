module HW1.T7
  (
    ListPlus (..)
  , Inclusive (..)
  , DotString (..)
  , Fun (..)
  ) where

data ListPlus a
  = a :+ ListPlus a
  | Last a
  deriving (Show)

--instance (Semigroup a, Semigroup b) => Semigroup (ListPlus a) where
--  (<>) (Last a) (Last b) =

data Inclusive a b = This a | That b | Both a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This b)      = This (a <> b)
  (<>) (That a) (That b)      = That (a <> b)
  (<>) (This a) (That b)      = Both a b
  (<>) (That a) (This b)      = Both b a
  (<>) (This a) (Both b c)    = Both (a <> b) c
  (<>) (Both a b) (This c)    = Both (a <> c) b
  (<>) (That a) (Both b c)    = Both b (a <> c)
  (<>) (Both a b) (That c)    = Both a (b <> c)
  (<>) (Both a b) (Both c d)  = Both (a <> c) (b <> d)

newtype DotString = DS String deriving (Show)

instance Monoid DotString where
  mempty = DS ""
  
instance Semigroup DotString where
  (<>) (DS "") b     = b
  (<>) a (DS "")     = a
  (<>) (DS a) (DS b) = DS(a ++ "." ++ b)

newtype Fun a = F (a -> a)

instance Monoid (Fun a) where
  mempty = (F id)
  
instance Semigroup (Fun a) where
  (<>) (F a) (F b) = F (a . b)