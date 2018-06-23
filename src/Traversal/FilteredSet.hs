{-# LANGUAGE TypeFamilies #-}

-- | Uses a set-based dictionary, but filtered to only words that can be
-- assembled from some permutation of the board. This is conservative, but
-- still reduces the dictionary by 40x or better when using the 1992 Boggle
-- dice set. (It often reduces the dictionary to less than 1/1000 its original
-- size.)
--
-- The down side of this approach is that we can't construct the dictionary set
-- until we've seen the board, so set construction time counts toward solve
-- time. But we *can* preprocess the dictionary in board-independent ways.
module Traversal.FilteredSet (T) where

import Control.Arrow ((&&&))
import Data.List (sort)
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq
import ByteStringUtil

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
  type CookedBoard T = (RawBoard, BS.ByteString)
  cookBoard = (id &&& (BS.pack . sort . ungrid))

  solve d (b, cs) =
    let d' = S.fromList $ [rw | (rw, sw) <- d, sw `isSubsequenceOf` cs]
    in uniqBy fst $
              [r | pos <- indices b
                 , r <- search d' b [pos] (BS.singleton (b `at` pos))
                 ]
