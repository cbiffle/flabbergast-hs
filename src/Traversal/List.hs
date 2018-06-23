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
import qualified Data.ByteString.Char8 as BS
import Uniq

-- | Yield all possible solutions of a board as a lazy list of "words" and
-- paths. Solutions will be legal per the rules but may not exist in the
-- dictionary.
possibleSolutions :: RawBoard -> Results
possibleSolutions b = [r | p <- indices b
                         , r <- dfs [p] $ BS.singleton $ b `at` p]
  where
    dfs path word = (word, ipath b $ path) :
                    [r | n <- neighborIndices b (last path)
                       , n `notElem` path
                       , let bc = b `at` n
                       , r <- dfs (path ++ [n]) (word `BS.snoc` bc)
                       ]

data T

instance Solver T where
  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = uniqBy fst $  -- because this *will* generate duplicate solutions
              filter ((`elem` map fst d) . fst) $  -- reject illegal words
              possibleSolutions b
