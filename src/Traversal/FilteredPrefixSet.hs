-- | Set-based filtered dictionary with prefix set.
module Traversal.FilteredPrefixSet (T) where

import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Control.Arrow ((&&&))
import Data.List (sort, tails)
import Control.DeepSeq (NFData(..))
import Base
import Uniq
import ByteStringUtil
import Traversal.Generic

data T

instance Solver T where
  solve d b =
    let cs = BS.pack $ sort $ ungrid b
        df = [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
        d' = S.fromList df
        pre = S.fromList $ [t | w <- df, t <- BS.inits w]
    in uniqBy fst $ paths (prefix (`S.member` pre) (`S.member` d')) b
