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
import Uniq

type Dictionary = T.Trie ()

-- Instance missing from package
instance NFData a => NFData (T.Trie a) where
  rnf = rnf . T.toList

search :: Dictionary -> RawBoard -> IPath -> B.ByteString -> Results
search d b path word
  | T.null d = []
  | otherwise =
  (if word `T.member` d then ((word, ipath b $ reverse path) :) else id)
  [r | i <- neighborIndices b (head path)
     , i `notElem` path
     , let bc = b `at` i
     , let p' = i : path
     , let w' = word `BC.snoc` bc
     , r <- search (w' `T.submap` d) b p' w'
     ]

data T

instance Solver T where
  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b =
    let trie = T.fromList $ fmap (\(w, _) -> (w, ())) d
    in uniqBy fst $
       [r | i <- indices b
          , r <- search trie b [i] (BC.singleton (b `at` i))
          ]
