-- | HAMT-based filtered dictionary with prefix testing in a single map.
module Traversal.FilteredPrefixHAMT (T) where

import qualified Data.HashMap.Strict as H
import Control.Arrow ((&&&))
import Data.List (sort)
import Control.DeepSeq (NFData(..), force)
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq
import ByteStringUtil
import Traversal.Generic

data T

prefixPattern = (True, False) : repeat (False, True)

instance Solver T where
  solve d b =
    let cs = BS.pack $ sort $ ungrid b
        df = [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
        d' = H.fromListWith (\(a, b) (c, d) -> force (a || c, b || d)) $
             [e | w <- df, e <- zip (w : init (BS.inits w)) prefixPattern]
        isPrefix w = snd $ H.lookupDefault (False, False) w d'
        isComplete w = fst $ H.lookupDefault (False, False) w d'
    in uniqBy fst $ paths (prefix isPrefix isComplete) b
