{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming.
--
-- This method is after Golam Kawsar's work here:
-- http://exceptional-code.blogspot.com/2012/02/solving-boggle-game-recursion-prefix.html
--
-- The idea is to do a fuzzy search using a cheap algorithm that might have
-- false positives (due to square reuse), and then do an expensive search only
-- if that succeeds. The fuzzy search runs in time O(n*n*w) for an n*n board
-- and a w-character word, so we run it for each word in the dictionary; the
-- length of the dictionary then dominates the scaling, making it effectively
-- linear in the dictionary size.
--
-- If the fuzzy search reports a potential match, we do the expensive search
-- for all instances of it.
--
-- This algorithm has the advantage of not producing duplicate words, so it
-- doesn't require post-filtering.
module DP.TwoPass (T) where

import Control.Arrow ((&&&))
import Data.List (sort, isSubsequenceOf, foldl')
import Data.Maybe (listToMaybe, maybeToList)
import Control.DeepSeq (NFData(..))
import Base

-- | Performs one step of the cheap scan, for one character of the target word.
-- Each element of 'last' indicates whether the corresponding location in 'b'
-- is the end of a valid path for the word, not including new character 'c'.
-- The output incorporates 'c' by checking next steps from each valid location
-- and ruling out any that don't contain 'c'.
--
-- This method is *almost* precise, but does not detect reuse of the same square
-- within a word, so there's some risk of false positive.
cheapStep :: RawBoard -> [[Bool]] -> Char -> [[Bool]]
cheapStep b last c =
  flip mapWithPos b $ \p bc -> bc == c
                               && or [last `tile` p' | p' <- nextSteps b p]

-- | Scans a board for potential places 'w' can occur.
cheap :: RawBoard -> String -> Bool
cheap b w = or $ concat $ foldl' (cheapStep b) trues w
    where trues = map (map (const True)) b

-- | Searches for an instance of word 'target' in 'b' using depth-first search.
-- This is nearly identical to my naive reference traversal.
expensive :: RawBoard -> String -> Maybe (String, Path)
expensive b target = listToMaybe $
                     [r | p <- positions b
                        , b `tile` p == head target
                        , r <- search (tail target) [p]]
  where
    search [] p = [(target, reverse p)]
    search (c : cs) path =
      [r | n <- nextSteps b (head path)
         , n `notElem` path
         , b `tile` n == c
         , r <- search cs (n : path)
         ]

data T

instance Solver T where
  type CookedDict T = RawDictionary
  cookDict = id

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | word <- d
                 , cheap b word
                 , r <- maybeToList $ expensive b word
                 ]
