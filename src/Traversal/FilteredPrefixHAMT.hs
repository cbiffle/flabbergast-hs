{-# LANGUAGE TypeFamilies #-}

-- | HAMT-based filtered dictionary with prefix testing in a single map.
module Traversal.FilteredPrefixHAMT (T) where

import qualified Data.HashMap.Strict as H
import Control.Arrow ((&&&))
import Data.List (sort, isSubsequenceOf, tails)
import Control.DeepSeq (NFData(..), force)
import Base
import Uniq

type Dict = H.HashMap String (Bool, Bool)

search :: Dict -> RawBoard -> Path -> String -> [(String, Path)]
search d b path word =
  let (complete, continue) = H.lookupDefault (False, False) word d
  in (if complete then ((reverse word, reverse path) :) else id)
     (if continue
        then [r | n <- nextSteps b (head path)
                , n `notElem` path
                , let p' = n : path
                , let w' = (b !! snd n !! fst n) : word
                , r <- search d b p' w'
                ]
        else [])

data T

tailPattern = (True, False) : repeat (False, True)

instance Solver T where
  type CookedDict T = [(String, String)]
                          -- reversed, sorted
  cookDict = fmap (reverse &&& sort)

  type CookedBoard T = (RawBoard, String)
  cookBoard b = (b, sort $ concat b)

  solve d (b, cs) =
    let df = [rw | (rw, sw) <- d, sw `isSubsequenceOf` cs]
        d' = H.fromListWith (\(a, b) (c, d) -> force (a || c, b || d)) $
             [e | rw <- df, e <- zip (tails rw) tailPattern]
    in uniqBy fst $
       [r | pos <- positions b
          , r <- search d' b [pos] [b !! snd pos !! fst pos]
          ]
