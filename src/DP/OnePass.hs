{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux.
--
-- Rather than using dynamic programming to implement a cheap filter as in
-- TwoPass, this uses it to generate actual paths, saving a pass.
module DP.OnePass (T) where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as S
import Base

type GridOf a = [[a]]

cheap' :: GridOf Char -> GridOf (S.Set Path) -> Char -> GridOf (S.Set Path)
cheap' b paths c =
  mapWithPos (\p bc -> S.fromList $
                       [p : path | bc == c
                                 , np <- nextSteps b p
                                 , path <- S.toList $ paths `tile` np
                                 , p `notElem` path]) b

cheap :: RawBoard -> String -> GridOf (S.Set Path)
cheap b (c : cs) = foldl' (cheap' b)
                          (mapWithPos
                                (\pos bc -> if bc == c
                                                then S.singleton [pos]
                                                else S.empty) b)
                          cs

search1 :: RawBoard -> String -> Maybe (String, Path)
search1 b w = fmap (\p -> (w, reverse p)) $
              listToMaybe $ concatMap S.toList $ concat $ cheap b w

data T

instance Solver T where
  type CookedDict T = RawDictionary
  cookDict = id

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | word <- d
                 , r <- maybeToList $ search1 b word
                 ]
