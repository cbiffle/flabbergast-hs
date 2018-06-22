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
import Base

type GridOf a = [[a]]

-- | Propagates potential paths, given a grid of known paths and the next
-- character in the word.
step :: GridOf Char -> GridOf (S.Set Path) -> Char -> GridOf (S.Set Path)
step b paths c = flip mapWithPos b $
    \p bc -> S.fromList $ [p : path | bc == c
                                    , np <- nextSteps b p
                                    , path <- S.toList $ paths `tile` np
                                    , p `notElem` path]

-- | Searches for instances of a single candidate word 'w' in 'b'. Returns one
-- such instance if found.
search1 :: RawBoard -> String -> Maybe (String, Path)
search1 b w@(c : cs) =
  fmap (\p -> (w, reverse p)) $
  listToMaybe $ concatMap S.toList $ concat $
  foldl' (step b) seed cs
  where seed = flip mapWithPos b $ \pos bc -> if bc == c
                                                then S.singleton [pos]
                                                else S.empty

data T

instance Solver T where
  type CookedDict T = RawDictionary
  cookDict = id

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | word <- d
                 , r <- maybeToList $ search1 b word
                 ]
