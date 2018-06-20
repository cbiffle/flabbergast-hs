{-# LANGUAGE TypeFamilies #-}

-- | Derived from B5 (set-based filtered dictionary) but using ByteString.
-- The way we're using strings, ByteString is not an obvious win.
module B11 where

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.ByteString.Lazy (ByteString)
import Control.Arrow ((&&&), first)
import Data.List (sort)
import Control.DeepSeq (NFData(..))
import Base

import B4 (SPath, emptyPath, extendPath, notInPath, pathList, pathHead)

isSubsequenceOf :: ByteString -> ByteString -> Bool
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

data SetBased

instance Solver SetBased where
  type CookedDict SetBased = [(ByteString, ByteString)] -- reversed, sorted
  cookDict = fmap ((B.reverse . BC.pack)  &&& (BC.pack . sort))

  type CookedBoard SetBased = CBoard
  cookBoard b = CBoard b $ BC.pack $ sort $ concat b

  solve d (CBoard b cs) =
    let d' = S.fromList $ [rw | (rw, sw) <- d, sw `isSubsequenceOf` cs]
    in [first BC.unpack r | pos <- positions b
                          , r <- search d' b (pos `extendPath` emptyPath)
                                       (BC.singleton (b !! snd pos !! fst pos))
                          ]