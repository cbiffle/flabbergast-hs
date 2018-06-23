{-# LANGUAGE TypeFamilies #-}

-- | HAMT-based filtered dictionary.
module Traversal.FilteredHAMT (T) where

import qualified Data.HashSet as H
import Control.Arrow ((&&&))
import Data.List (sort)
import Control.DeepSeq (NFData(..), force)
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq
import ByteStringUtil

type Dict = H.HashSet BS.ByteString

search :: Dict -> RawBoard -> IPath -> BS.ByteString -> Results
search d b path word =
  (if word `H.member` d then ((word, ipath b $ reverse path) :) else id)
  [r | n <- neighborIndices b (head path)
     , n `notElem` path
     , let p' = n : path
     , let w' = word `BS.snoc` (b `at` n)
     , r <- search d b p' w'
     ]

data T

instance Solver T where
  type CookedBoard T = (RawBoard, BS.ByteString)
  cookBoard b = (b, BS.pack $ sort $ ungrid b)

  solve d (b, cs) =
    let d' = H.fromList [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
    in uniqBy fst $
       [r | pos <- indices b
          , r <- search d' b [pos] $ BS.singleton $ b `at` pos
          ]
