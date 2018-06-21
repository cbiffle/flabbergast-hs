{-# LANGUAGE TypeFamilies #-}
-- | Trie-based full dictionary using ByteStrings and dictionary subsetting.
module Traversal.FilteredTrie (T) where

import Control.DeepSeq (NFData(..))
import qualified Data.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (group, sort)
import Base

type Dictionary = T.Trie ()

isSubsequenceOf :: BC.ByteString -> BC.ByteString -> Bool
isSubsequenceOf as bs
  | B.null as = True
  | B.null bs = False
  | otherwise =
      let Just (a, as') = B.uncons as
          Just (b, bs') = B.uncons bs
      in case a `compare` b of
          EQ -> as' `isSubsequenceOf` bs'
          GT -> as `isSubsequenceOf` bs'
          LT -> False

search :: Dictionary -> RawBoard -> Path -> B.ByteString -> [(String, Path)]
search d b path word
  | T.null d = []
  | otherwise =
    (if word `T.member` d then ((BC.unpack word, path) :) else id)
    [r | n <- nextSteps b (head path)
       , n `notElem` path
       , let bc = b !! snd n !! fst n
       , let p' = n : path
       , let w' = word `BC.snoc` bc
       , r <- search (w' `T.submap` d) b p' w'
       ]

data T

instance Solver T where
  type CookedDict T = [BC.ByteString]
  cookDict = fmap BC.pack

  type CookedBoard T = (RawBoard, BC.ByteString)
  cookBoard b = (b, BC.pack $ sort $ concat b)

  solve d (b, cs) = 
    let d' = T.fromList $ [(w, ()) | w <- d, w `isSubsequenceOf` cs]
    in [r | pos <- positions b
          , r <- search d' b [pos]
                            (BC.singleton (b !! snd pos !! fst pos))
                 ]
