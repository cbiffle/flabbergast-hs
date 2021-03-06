-- | A very high level, fairly naive implementation: exhaustive enumerate every
-- possible path through the board, and check them against a (linked list)
-- dictionary.
--
-- This pretends I don't know how Haskell lists work, so it puts new items at
-- the end of the list and repeatedly inspects the lsat element.
--
-- This is, as you might guess, not very fast. It is, however, not as slow as I
-- expected.
module Traversal.List (solver) where

import Base
import Uniq

import Traversal.Generic

solver :: Solver
solver d = uniqBy fst .  -- because this *will* generate duplicate solutions
           paths (exhaustive (`elem` map fst d))
