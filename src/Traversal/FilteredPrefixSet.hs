-- | Set-based filtered dictionary with prefix set.
module Traversal.FilteredPrefixSet (solver) where

import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Base
import Uniq
import ByteStringUtil
import Traversal.Generic

solver :: Solver
solver d b =
  let cs = BS.pack $ sort $ ungrid b
      df = [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
      d' = S.fromList df
      pre = S.fromList $ [t | w <- df, t <- BS.inits w]
  in uniqBy fst $ paths (prefix (`S.member` pre) (`S.member` d')) b
