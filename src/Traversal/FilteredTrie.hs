{-# LANGUAGE TypeFamilies #-}
-- | Trie-based full traversal using ByteStrings and dictionary subsetting.
module Traversal.FilteredTrie (T) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData(..))
import qualified Data.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (group, sort)
import Base
import Uniq
import ByteStringUtil

type Dictionary = T.Trie ()

search :: Dictionary -> RawBoard -> Path -> B.ByteString -> [(String, Path)]
search d b path word
  | T.null d = []
  | otherwise =
    (if word `T.member` d then ((BC.unpack word, reverse path) :) else id)
    [r | n <- nextSteps b (head path)
       , n `notElem` path
       , let bc = b !! snd n !! fst n
       , let p' = n : path
       , let w' = word `BC.snoc` bc
       , r <- search (w' `T.submap` d) b p' w'
       ]

data T

instance Solver T where
  type CookedDict T = [(BC.ByteString, BC.ByteString)] -- packed, sorted
  cookDict = fmap (BC.pack &&& (BC.pack . sort))

  type CookedBoard T = (RawBoard, BC.ByteString)
  cookBoard b = (b, BC.pack $ sort $ concat b)

  solve d (b, cs) = 
    let d' = T.fromList $ [(w, ()) | (w, sw) <- d, sw `isSubsequenceOf` cs]
    in uniqBy fst $
       [r | pos <- positions b
          , r <- search d' b [pos]
                            (BC.singleton (b !! snd pos !! fst pos))
                 ]
