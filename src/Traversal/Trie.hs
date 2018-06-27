-- | Trie-based full dictionary, using custom trie.
--
-- Customized structure here gives slightly better construction and lookup
-- performance than the off-the-shelf options.
module Traversal.Trie (T) where

import qualified Data.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as H
import Data.List (groupBy)
import Data.Maybe (maybeToList)
import Base
import Uniq

data Trie = Node !(Maybe B.ByteString)  -- ^ Word that ends here.
                 !(H.HashMap Char Trie) -- ^ Continuations from here.
    deriving (Show)

completed (Node mw _) = mw

tlookup c (Node _ m) = H.lookup c m

fromSortedList :: [B.ByteString] -> Trie
fromSortedList = level 0
  where
    level :: Int -> [B.ByteString] -> Trie
    level _ [] = Node Nothing H.empty
    level lvl (w : ws)
      | B.length w == lvl = Node (Just w) (proc ws)
      | otherwise         = Node Nothing (proc (w : ws))
      where
        proc = H.fromList .
               map (\ch -> (BC.index (head ch) lvl, level (lvl + 1) ch)) .
               groupBy (\a b -> BC.index a lvl == BC.index b lvl)

search :: RawBoard -> Trie -> IPath -> Results
search b t path =
  (maybe id (\w -> ((w, ipath b $ reverse path) :)) (completed t))
  [r | i <- neighborIndices b (head path)
     , i `notElem` path
     , let bc = b `at` i
     , t' <- maybeToList $ tlookup bc t
     , r <- search b t' (i : path)
     ]

data T

instance Solver T where
  solve d b =
    let trie = fromSortedList $ map fst d
    in uniqBy fst $
       [r | i <- indices b
          , t <- maybeToList $ tlookup (b `at` i) trie
          , r <- search b t [i]
          ]
