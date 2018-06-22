{-# LANGUAGE TypeFamilies #-}

-- | Derived from FilteredSetPath but using ByteString.
-- The way we're using strings, ByteString is not an obvious win.
module Traversal.FilteredByteStringSet (T) where

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Control.Arrow ((&&&), first)
import Data.List (sort)
import Control.DeepSeq (NFData(..))
import Base
import Uniq
import ByteStringUtil

import Traversal.SetPath
  (SPath, emptyPath, extendPath, notInPath, pathList, pathHead)

type BDict = S.Set ByteString

search :: BDict -> RawBoard -> SPath -> ByteString -> [(ByteString, Path)]
search d b path word =
  (if word `S.member` d then ((B.reverse word, pathList path) :) else id)
  [r | n <- nextSteps b (pathHead path)
     , n `notInPath` path
     , let p' = n `extendPath` path
     , let w' = (b !! snd n !! fst n) `BC.cons` word
     , r <- search d b p' w'
     ]

data CBoard = CBoard !RawBoard -- ^ The board.
                     !ByteString  -- ^ The sorted characters of the board.

instance NFData CBoard where
  rnf (CBoard b cs) = rnf (b, cs)

data T

instance Solver T where
  type CookedDict T = [(ByteString, ByteString)] -- reversed, sorted
  cookDict = fmap ((B.reverse . BC.pack)  &&& (BC.pack . sort))

  type CookedBoard T = CBoard
  cookBoard b = CBoard b $ BC.pack $ sort $ concat b

  solve d (CBoard b cs) =
    let d' = S.fromList $ [rw | (rw, sw) <- d, sw `isSubsequenceOf` cs]
    in uniqBy fst $
       [first BC.unpack r | pos <- positions b
                          , r <- search d' b (pos `extendPath` emptyPath)
                                       (BC.singleton (b !! snd pos !! fst pos))
                          ]
