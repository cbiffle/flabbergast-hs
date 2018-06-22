{-# LANGUAGE TypeFamilies #-}

-- | HAMT-based filtered dictionary with prefix testing, using ByteString.
module Traversal.FilteredPrefixBSHAMT (T) where

import qualified Data.HashMap.Strict as H
import Control.Arrow ((&&&))
import Data.List (sort, tails)
import Control.DeepSeq (NFData(..), force)
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq
import ByteStringUtil

type Dict = H.HashMap BS.ByteString (Bool, Bool)

search :: Dict -> RawBoard -> Path -> BS.ByteString -> [(String, Path)]
search d b path word =
  let (complete, continue) = H.lookupDefault (False, False) word d
  in (if complete then ((BS.unpack word, reverse path) :) else id)
     (if continue
        then [r | n <- nextSteps b (head path)
                , n `notElem` path
                , let p' = n : path
                , let w' = word `BS.snoc` (b !! snd n !! fst n)
                , r <- search d b p' w'
                ]
        else [])

data T

prefixPattern = (True, False) : repeat (False, True)

instance Solver T where
  type CookedDict T = [(BS.ByteString, BS.ByteString)]
                          -- word, sorted
  cookDict = fmap (\w -> let bw = BS.pack w in (bw, BS.sort bw))

  type CookedBoard T = (RawBoard, BS.ByteString)
  cookBoard b = (b, BS.pack $ sort $ concat b)

  solve d (b, cs) =
    let df = [w | (w, sw) <- d, sw `isSubsequenceOf` cs]
        d' = H.fromListWith (\(a, b) (c, d) -> force (a || c, b || d)) $
             [e | w <- df, e <- zip (w : init (BS.inits w)) prefixPattern]
    in uniqBy fst $
       [r | pos <- positions b
          , r <- search d' b [pos] (BS.singleton (b !! snd pos !! fst pos))
          ]
