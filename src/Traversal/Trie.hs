{-# LANGUAGE TypeFamilies #-}
-- | Trie-based full dictionary using ByteStrings.
--
-- I'm not using ByteStrings here because I think they'll confer any particular
-- performance advantage -- in this use case, they don't. I'm using them because
-- there's an off-the-shelf Trie implementation for them that meets my needs.
module Traversal.Trie (T) where

import Control.DeepSeq (NFData(..))
import qualified Data.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Base

type Dictionary = T.Trie ()

-- Instance missing from package
instance NFData a => NFData (T.Trie a) where
  rnf = rnf . T.toList

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
  type CookedDict T = Dictionary
  cookDict = T.fromList . fmap (\s -> (BC.pack s, ()))

  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b = [r | pos <- positions b
                 , r <- search d b [pos]
                                   (BC.singleton (b !! snd pos !! fst pos))
                 ]
