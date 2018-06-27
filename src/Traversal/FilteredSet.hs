-- | Uses a set-based dictionary, but filtered to only words that can be
-- assembled from some permutation of the board. This is conservative, but
-- still reduces the dictionary by 40x or better when using the 1992 Boggle
-- dice set. (It often reduces the dictionary to less than 1/1000 its original
-- size.)
--
-- The down side of this approach is that we can't construct the dictionary set
-- until we've seen the board, so set construction time counts toward solve
-- time. But we *can* preprocess the dictionary in board-independent ways.
module Traversal.FilteredSet (solver) where

import Data.List (sort)
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq
import ByteStringUtil
import Traversal.Generic

solver :: Solver
solver d b =
  let cs = BS.pack $ sort $ ungrid b
      d' = S.fromList $ [rw | (rw, sw) <- d, sw `isSubsequenceOf` cs]
  in uniqBy fst $ paths (exhaustive (`S.member` d')) b
