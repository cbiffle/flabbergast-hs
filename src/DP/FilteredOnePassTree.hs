{-# LANGUAGE BangPatterns #-}

-- | Dynamic programming, redux^3. Use a specialized data structure intended to
-- improve step performance and reduce allocation.
module DP.FilteredOnePassTree (T) where

import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.ByteString.Char8 as BS
import Base
import ByteStringUtil

-- | A data structure that provides for efficiently merging paths, and which
-- can't be (easily) used to construct an invalid path as long as 'Start' is
-- only used at the beginning.
data PathTree = Start {-# UNPACK #-} !Int -- ^ Tile index of first letter.
              | Snoc {-# UNPACK #-} !Int  -- ^ Tile index of add'l letter.
                     {-# UNPACK #-} !(NE.NonEmpty PathTree)
                      -- ^ Paths we are continuing.
  deriving (Eq, Ord, Show)

i `notInTree` Start i' = i /= i'
i `notInTree` Snoc i' ps = i /= i' && all (i `notInTree`) ps

-- | Extracts an arbitrary path from the tree. (Since all paths are equivalent
-- for scoring purposes, any one will do.)
takeIPath :: PathTree -> IPath
takeIPath = iter []
  where iter !acc (Start i) = i : acc
        iter !acc (Snoc i ps) = iter (i : acc) (NE.head ps)

-- | Propagates potential paths, given a grid of known paths and the next
-- character in the word.
step :: RawBoard -> GridOf (Maybe PathTree) -> Char -> GridOf (Maybe PathTree)
step b paths c = b `gfor` \i bc ->
  let neighborPaths = [path | bc == c
                            , ni <- neighborIndices b i
                            , path <- maybeToList $ paths `at` ni
                            , i `notInTree` path]
  in Snoc i <$> NE.nonEmpty neighborPaths

-- | Searches for instances of a single candidate word 'w' in 'b'. Returns one
-- such instance if found.
search1 :: RawBoard -> BS.ByteString -> Maybe (BS.ByteString, Path)
search1 b w =
  fmap (\p -> (w, ipath b $ takeIPath p)) $
  listToMaybe $ catMaybes $ ungrid $
  BS.foldl' (step b) seed (BS.tail w)
  where seed = b `gfor` \i c -> if c == BS.head w
                                  then Just (Start i)
                                  else Nothing

data T

instance Solver T where
  solve d b =
    let cs = BS.sort $ BS.pack $ ungrid b
    in [r | (word, sorted) <- d
          , sorted `isSubsequenceOf` cs
          , r <- maybeToList $ search1 b word
          ]
