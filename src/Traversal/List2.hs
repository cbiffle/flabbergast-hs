{-# LANGUAGE TypeFamilies #-}
-- | A slightly cleverer exhaustive DFS that uses the "reversed accumulator"
-- pattern to avoid allocating and traversing lists as often.
--
-- For reasons I don't understand (because I'm not interested enough in this
-- algorithm to fully investigate) this is slower than the naive DFS.
module Traversal.List2 (T) where

import Base
import Uniq
import Control.Arrow ((&&&))
import Control.DeepSeq (force)

-- | Yield all possible solutions of a board as a lazy list of "words" and
-- paths. Solutions will be legal per the rules but may not exist in the
-- dictionary.
possibleSolutions :: RawBoard -> [(String, Path)]
possibleSolutions b = [r | p <- positions b
                         , r <- dfs [p] [b !! snd p !! fst p]]
  where
    dfs path word = (word, path) :
                    [r | n <- nextSteps b (head path)
                       , n `notElem` path
                       , let bc = b !! snd n !! fst n
                       , r <- dfs (n : path) (bc : word)
                       ]

data T

instance Solver T where
  type CookedDict T = RawDictionary
  cookDict = fmap reverse

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = map (\(w, p) -> (reverse w, reverse p)) $
              uniqBy fst $  -- because this *will* generate duplicate solutions
              filter ((`elem` d) . fst) $  -- reject illegal words
              possibleSolutions b
