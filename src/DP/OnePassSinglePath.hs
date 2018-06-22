{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux^2.
--
-- This exploits the fact that a word only counts for points once per board,
-- by not recording multiple paths per square.
module DP.OnePassSinglePath (T) where

import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import Base

type GridOf a = [[a]]

-- | One step of the search, phrased as a fold. Given a grid of valid paths for
-- a word prefix, tries to extend those paths to include a neighboring instance
-- of character 'c', or discard them if they cannot be extended.
--
-- If duplicate paths reach a tile, their futures are identical, so we
-- arbitrarily choose one to survive and discard the rest without loss of
-- generality. (This is implicit in the use of 'listToMaybe'.)
step :: GridOf Char -> GridOf (Maybe Path) -> Char -> GridOf (Maybe Path)
step b paths c = flip mapWithPos b $ \p bc ->
  listToMaybe $
  [p : path | bc == c
            , np <- nextSteps b p
            , path <- maybeToList $ paths `tile` np
            , p `notElem` path]

-- | Searches 'b' for an instance of 'w', returning one if found.
search1 :: RawBoard -> String -> Maybe (String, Path)
search1 b w = fmap (\p -> (w, reverse p)) $
              listToMaybe $ catMaybes $ concat $
              foldl' (step b) seed (tail w)
  where
    seed = flip mapWithPos b $ \pos bc -> if bc == head w
                                            then Just [pos]
                                            else Nothing

data T

instance Solver T where
  type CookedDict T = RawDictionary
  cookDict = id

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | word <- d, r <- maybeToList $ search1 b word]
