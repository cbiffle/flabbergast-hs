{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux.
--
-- Instead of a fuzzy DP solution followed by an exact DFS, this is an exact
-- DP solution. In each step, it propagates all valid paths through a given
-- tile for a given word. By maintaining the path information it can detect,
-- and reject, self-intersecting paths.
module DP.OnePass (T) where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Base

-- | Propagates potential paths, given a grid of known paths and the next
-- character in the word.
step :: RawBoard -> GridOf (S.Set IPath) -> Char -> GridOf (S.Set IPath)
step b paths c = b `gfor` \i bc ->
  S.fromList $ [i : path | bc == c
                         , ni <- neighborIndices b i
                         , path <- S.toList $ paths `at` ni
                         , i `notElem` path]

-- | Searches for instances of a single candidate word 'w' in 'b'. Returns one
-- such instance if found.
search1 :: RawBoard -> BS.ByteString -> Maybe (BS.ByteString, Path)
search1 b w =
  fmap (\p -> (w, ipath b $ reverse p)) $
  listToMaybe $ concatMap S.toList $ ungrid $
  BS.foldl' (step b) seed (BS.tail w)
  where seed = b `gfor` \i bc -> if bc == BS.head w
                                   then S.singleton [i]
                                   else S.empty

data T

instance Solver T where
  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | (word, _) <- d
                 , r <- maybeToList $ search1 b word
                 ]
