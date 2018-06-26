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

type WordSet = S.Set BS.ByteString

search :: WordSet -> WordSet -> RawBoard -> IPath -> BS.ByteString -> Results
search d pre b path word =
  (if word `S.member` d then ((word, ipath b $ reverse path) :) else id)
  [r | n <- neighborIndices b (head path)
     , n `notElem` path
     , let (p', w') = (n : path, word `BS.snoc` (b `at` n))
     , w' `S.member` pre
     , r <- search d pre b p' w'
     ]

data T

instance Solver T where
  solve d b =
    let cs = BS.pack $ sort $ ungrid b
        df = [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
        d' = S.fromList df
        pre = S.fromList $ [t | w <- df, t <- BS.inits w]
    in uniqBy fst $
       [r | pos <- indices b
          , r <- search d' pre b [pos]
                                 (BS.singleton (b `at` pos))
                 ]
