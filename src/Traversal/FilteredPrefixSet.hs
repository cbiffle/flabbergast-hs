{-# LANGUAGE TypeFamilies #-}

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
  type CookedDict T = [(BS.ByteString, BS.ByteString, [BS.ByteString])]
                          -- identity, sorted, prefixes
  cookDict = fmap (\w -> (w, BS.sort w, BS.inits w))

  type CookedBoard T = (RawBoard, BS.ByteString)
  cookBoard = (id &&& (BS.pack . sort . ungrid))

  solve d (b, cs) =
    let df = [e | e@(_, sw, _) <- d, sw `isSubsequenceOf` cs]
        d' = S.fromList $ [rw | (rw, _, _) <- df]
        pre = S.fromList $ [t | (_, _, ts) <- df, t <- ts]
    in uniqBy fst $
       [r | pos <- indices b
          , r <- search d' pre b [pos]
                                 (BS.singleton (b `at` pos))
                 ]
