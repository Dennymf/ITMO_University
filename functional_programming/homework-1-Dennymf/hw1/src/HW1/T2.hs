module HW1.T2
  (
    N (..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import GHC.Natural

data N
  = Z
  | S N
  deriving Show

nplus :: N -> N -> N
nplus Z a         = a
nplus a Z         = a
nplus a (S b)     = nplus (S a) b

nmult :: N -> N -> N
nmult Z _         = Z
nmult _ Z         = Z
nmult a (S b)     = nplus a (nmult a b)

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) =
  if ncmp (S a) (S b) == LT
  then Nothing
  else nsub a b

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = (S (nFromNatural (pred n)))

nToNum :: Num a => N -> a
nToNum (S a) = 1 + nToNum a
nToNum Z     = 0

nEven:: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S a)) = nEven a

nOdd :: N -> Bool
nOdd = not . nEven

getValue :: Maybe N -> N
getValue (Just a) = a
getValue Nothing = Z

ndiv :: N -> N -> N
ndiv _ Z = error "Division by zero"
ndiv a b = ndiv' a b Z where
  ndiv' a' b' n
    | ncmp a' b' == LT = n
    | otherwise = ndiv' (getValue(nsub a' b')) b (S n)

nmod :: N -> N -> N
nmod a b = nmod' a b where
  nmod' a' b'
    | ncmp a' b' == LT = a'
    | otherwise = nmod' (getValue(nsub a' b')) b