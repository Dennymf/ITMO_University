module HW1.T4
  (
    tfoldr
  , treeToList
  ) where

import HW1.T3 (Tree (..))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ res Leaf                    = res
tfoldr func acc (Branch _ l value r) = tfoldr func (func value (tfoldr func acc r)) l

treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList tree = tfoldr (:) [] tree