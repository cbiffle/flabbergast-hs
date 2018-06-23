{-# LANGUAGE TypeFamilies #-}

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

type Dict = H.HashMap BS.ByteString (Bool, Bool)

search :: Dict -> RawBoard -> IPath -> BS.ByteString -> Results
search d b path word =
  let (complete, continue) = H.lookupDefault (False, False) word d
  in (if complete then ((word, ipath b $ reverse path) :) else id)
     (if continue
        then [r | n <- neighborIndices b (head path)
                , n `notElem` path
                , let p' = n : path
                , let w' = word `BS.snoc` (b `at` n)
                , r <- search d b p' w'
                ]
        else [])

data T

prefixPattern = (True, False) : repeat (False, True)

instance Solver T where
  type CookedDict T = [(BS.ByteString, BS.ByteString)]
                          -- normal, sorted
  cookDict = fmap (id &&& BS.sort)

  type CookedBoard T = (RawBoard, BS.ByteString)
  cookBoard b = (b, BS.pack $ sort $ ungrid b)

  solve d (b, cs) =
    let df = [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
        d' = H.fromListWith (\(a, b) (c, d) -> force (a || c, b || d)) $
             [e | w <- df, e <- zip (w : init (BS.inits w)) prefixPattern]
    in uniqBy fst $
       [r | pos <- indices b
          , r <- search d' b [pos] $ BS.singleton $ b `at` pos
          ]
