{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux^2.
--
-- This exploits the fact that a word only counts for points once per board,
-- by not recording multiple paths per square.
module B8 (DP) where

import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import Base

type GridOf a = [[a]]

cheap' :: GridOf Char -> GridOf (Maybe Path) -> Char -> GridOf (Maybe Path)
cheap' b paths c =
  mapWithPos (\p bc -> listToMaybe $
                       [p : path | bc == c
                                 , np <- nextSteps b p
                                 , path <- maybeToList $ paths `tile` np
                                 , p `notElem` path]) b

cheap :: RawBoard -> String -> GridOf (Maybe Path)
cheap b (c : cs) = foldl' (cheap' b)
                          (mapWithPos
                                (\pos bc -> if bc == c
                                                then Just [pos]
                                                else Nothing) b)
                          cs

search1 :: RawBoard -> String -> Maybe (String, Path)
search1 b w = fmap (\p -> (w, reverse p)) $
             listToMaybe $ catMaybes $ concat $ cheap b w

data DP

instance Solver DP where
  type CookedDict DP = RawDictionary
  cookDict = id

  type CookedBoard DP = RawBoard
  cookBoard = id

  solve d b = [r | word <- d
                 , r <- maybeToList $ search1 b word
                 ]
