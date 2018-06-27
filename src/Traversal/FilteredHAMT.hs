-- | HAMT-based filtered dictionary.
module Traversal.FilteredHAMT (solver) where

import qualified Data.HashSet as H
import Control.Arrow ((&&&))
import Data.List (sort)
import Control.DeepSeq (NFData(..), force)
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq
import ByteStringUtil
import Traversal.Generic

solver :: Solver
solver d b =
  let cs = BS.pack $ sort $ ungrid b
      d' = H.fromList [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
  in uniqBy fst $ paths (exhaustive (`H.member` d')) b
