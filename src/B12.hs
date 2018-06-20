{-# LANGUAGE TypeFamilies #-}
-- | Trie-based full dictionary using ByteStrings.
module B12 where

import qualified Data.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Base

import B4 (SPath, emptyPath, extendPath, notInPath, pathList, pathHead)

type Dictionary = T.Trie ()

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

data TrieBased

instance Solver TrieBased where
  type CookedDict TrieBased = Dictionary
  cookDict = T.fromList . fmap (\s -> (BC.pack s, ()))

  type CookedBoard TrieBased = RawBoard
  cookBoard = id

  solve d b = [r | pos <- positions b
                 , r <- search d b (pos `extendPath` emptyPath)
                                   (BC.singleton (b !! snd pos !! fst pos))
                 ]
