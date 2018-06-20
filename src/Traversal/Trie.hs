{-# LANGUAGE TypeFamilies #-}
-- | Trie-based full dictionary using ByteStrings.
module Traversal.Trie (T) where

import Control.DeepSeq (NFData(..))
import qualified Data.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Base

import Traversal.SetPath
  (SPath, emptyPath, extendPath, notInPath, pathList, pathHead)

type Dictionary = T.Trie ()

instance NFData a => NFData (T.Trie a) where
  rnf = rnf . T.toList

search :: Dictionary -> RawBoard -> SPath -> B.ByteString -> [(String, Path)]
search d b path word
  | T.null d = []
  | otherwise =
  (if word `T.member` d then ((BC.unpack word, pathList path) :) else id)
  [r | n <- nextSteps b (pathHead path)
     , n `notInPath` path
     , let bc = b !! snd n !! fst n
     , let p' = n `extendPath` path
     , let w' = word `BC.snoc` bc
     , r <- search (w' `T.submap` d) b p' w'
     ]

data T

instance Solver T where
  type CookedDict T = Dictionary
  cookDict = T.fromList . fmap (\s -> (BC.pack s, ()))

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | pos <- positions b
                 , r <- search d b (pos `extendPath` emptyPath)
                                   (BC.singleton (b !! snd pos !! fst pos))
                 ]
