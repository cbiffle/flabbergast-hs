{-# LANGUAGE TypeFamilies #-}
-- | A very high level, fairly naive implementation: exhaustive enumerate every
-- possible path through the board, and check them against a (linked list)
-- dictionary.
--
-- This pretends I don't know how Haskell lists work, so it puts new items at
-- the end of the list and repeatedly inspects the lsat element.
--
-- This is, as you might guess, not very fast. It is, however, not as slow as I
-- expected.
module Traversal.List (T) where

import Base
import Uniq

-- | Yield all possible solutions of a board as a lazy list of "words" and
-- paths. Solutions will be legal per the rules but may not exist in the
-- dictionary.
possibleSolutions :: RawBoard -> [(String, Path)]
possibleSolutions b = [r | p <- positions b
                         , r <- dfs [p] [b !! snd p !! fst p]]
  where
    dfs path word = (word, path) :
                    [r | n <- nextSteps b (last path)
                       , n `notElem` path
                       , let bc = b !! snd n !! fst n
                       , r <- dfs (path ++ [n]) (word ++ [bc])
                       ]

data T

instance Solver T where
  type CookedDict T = RawDictionary
  cookDict = id

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = uniqBy fst $  -- because this *will* generate duplicate solutions
              filter ((`elem` d) . fst) $  -- reject illegal words
              possibleSolutions b
