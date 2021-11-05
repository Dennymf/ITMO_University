module HW1.T3
  (
    Tree (..)
  , tmember
  , tinsert
  , tsize
  , tdepth
  , tFromList
  ) where

data Tree a
  = Leaf
  | Branch Int (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch size _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ l _ r) = 1 + max (tdepth l) (tdepth r)

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember element (Branch _ l value r)
    | element == value = True
    | element < value  = tmember element l
    | otherwise        = tmember element r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert element Leaf = Branch 1 Leaf element Leaf
tinsert element (Branch size l value r)
    | element < value = rotate $ mkBranch (tinsert element l) value r
    | element > value = rotate $ mkBranch l value (tinsert element r)
    | otherwise       = (Branch size l value r)

tFromList :: Ord a => [a] -> Tree a
tFromList xs = tFromList' xs Leaf where
  tFromList' (x:xs') t = tFromList' xs' (tinsert x t)
  tFromList' [] t     = t

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l value r = Branch ((tsize l) + (tsize r) + 1) l value r

rotate :: Tree a -> Tree a
rotate Leaf = Leaf
rotate (Branch _ l value (Branch rsize c rvalue rr))
  | (tdepth (Branch rsize c rvalue rr) - tdepth l) == 2 && (tdepth c <= tdepth rr) =
    Branch ((tsize l) + (tsize c) + (tsize rr) + 2)
    (Branch
    ((tsize l) + (tsize c) + 1)
      l
      value
      c
    )
    rvalue
    rr
rotate (Branch _ (Branch lsize ll lvalue c) value r)
  | (tdepth (Branch lsize ll lvalue c) - tdepth r) == 2 && (tdepth c <= tdepth ll) =
    Branch ((tsize r) + (tsize c) + (tsize ll) + 2)
    ll
    lvalue
    (Branch
      ((tsize r) + (tsize c) + 1)
      c
      value
      r
    )
rotate (Branch _ l value (Branch rsize (Branch rlsize m rlvalue n) rvalue rr))
  | (tdepth (Branch rsize (Branch rlsize m rlvalue n) rvalue rr) - tdepth l == 2)
    && (tdepth (Branch rlsize m rlvalue n) > tdepth rr) =
    Branch ((tsize l) + (tsize m) + (tsize n) + (tsize rr) + 3)
    (Branch
      ((tsize l) + (tsize m) + 1)
      l
      value
      m
    )
    rlvalue
    (Branch
      ((tsize n) + (tsize rr) + 1)
      n
      rvalue
      rr
    )
rotate (Branch _ (Branch lsize ll lvalue (Branch lrsize m lrvalue n)) value r)
  | (tdepth (Branch lsize ll lvalue (Branch lrsize m lrvalue n)) - tdepth r == 2)
    && (tdepth (Branch lrsize m lrvalue n) > tdepth ll) =
    Branch ((tsize ll) + (tsize m) + (tsize n) + (tsize r) + 3)
    (Branch
      ((tsize ll) + (tsize m) + 1)
      ll
      lvalue
      m
    )
    lrvalue
    (Branch
      ((tsize n) + (tsize r) + 1)
      n
      value
      r
    )
rotate (Branch size l value r) =
  Branch size l value r