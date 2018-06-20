{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux.
--
-- Rather than using dynamic programming to implement a cheap filter as in B6,
-- this uses it to generate actual paths.
module B7 (DP) where

import Control.Arrow ((&&&))
import Data.List (foldl')
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

search1 :: RawBoard -> String -> [(String, Path)]
search1 b w = map (\p -> (w, reverse p)) $
             S.toList $ S.unions $ concat $ cheap b w

data DP

instance Solver DP where
  type CookedDict DP = RawDictionary
  cookDict = id

  type CookedBoard DP = RawBoard
  cookBoard = id

  solve d b = [r | word <- d
                 , r <- search1 b word
                 ]
