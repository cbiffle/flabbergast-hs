{-# LANGUAGE TypeFamilies #-}
module Traversal.Set (T) where

import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq

type Dictionary = S.Set BS.ByteString

search :: Dictionary -> RawBoard -> IPath -> BS.ByteString -> Results
search d b path word =
  (if word `S.member` d then ((word, ipath b $ reverse path) :)
                        else id)
  [r | n <- neighborIndices b (head path)
     , n `notElem` path
     , let (p', w') = (n : path, word `BS.snoc` (b `at` n))
     , r <- search d b p' w'
     ]

data T

instance Solver T where
  type CookedBoard T = RawBoard
  cookBoard = id

  solve d b =
    let set = S.fromList $ map fst d
    in uniqBy fst $
       [r | pos <- indices b
          , r <- search set b [pos] (BS.singleton (b `at` pos))
          ]
