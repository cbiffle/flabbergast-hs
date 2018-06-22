{-# LANGUAGE TypeFamilies #-}

-- | Dynamic programming, redux^5.
--
-- Filter the dictionary to the board, with bytestrings.
module DP.Filtered1PVBS (T) where

import Control.Arrow ((&&&))
import Control.DeepSeq(NFData(..))
import Data.List (foldl', transpose, sort)
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Control.Monad (join)
import Base
import ByteStringUtil
import GridOf

type IPath = [Int]

-- | Propagate possible paths by one step, for the next character 'c'.
step :: GridOf Char -> GridOf (Maybe IPath) -> Char -> GridOf (Maybe IPath)
step g@(GO _ _ gn) (GO paths _ _) c = g `gfor` \i bc -> listToMaybe $
      [i : path | bc == c
                , ni <- gn V.! i
                , path <- maybeToList $ paths V.! ni
                , i `notElem` path
                ]

-- | Searches for instances of a single candidate word 'w' in 'b'. Returns one
-- such instance if found.
search1 :: GridOf Char -> BS.ByteString -> Maybe (String, Path)
search1 b w =
  fmap (\p -> (BS.unpack w, map (goPositions b V.!) (reverse p))) $
  listToMaybe $ catMaybes $ V.toList $ goVec $
  BS.foldl' (step b) seed $
  BS.tail w
  where
    seed = b `gfor` \i bc -> if bc == BS.head w
                               then Just [i]
                               else Nothing

data T

instance Solver T where
  type CookedDict T = [(BS.ByteString, BS.ByteString)]
  cookDict = fmap (BS.pack &&& (BS.pack . sort))

  type CookedBoard T = (GridOf Char, BS.ByteString)
  cookBoard = mkGridOf &&& (BS.pack . sort . concat)
                
  solve d (b, cs) =
    let d' = [w | (w, s) <- d, s `isSubsequenceOf` cs]
    in [r | word <- d', r <- maybeToList $ search1 b word]
