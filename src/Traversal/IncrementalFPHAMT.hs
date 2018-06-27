-- | Filtered prefix HAMT using cheaply allocatable data structures with
-- consistent incremental hashing.
--
-- ByteString 'snoc' is fast, but allocates a complete copy of the string.
-- Hashing a ByteString for insertion or membership check is also O(n) in the
-- length of the word.
--
-- This implementation uses a list-like structure with O(1) 'snoc' and 'hash'
-- to reduce the overhead of both candidate construction and membership checks.
module Traversal.IncrementalFPHAMT (solver) where

import qualified Data.HashMap.Strict as H
import Data.Hashable
import Control.Arrow ((&&&))
import Data.List (sort, foldl')
import Control.DeepSeq (NFData(..), force)
import qualified Data.ByteString.Char8 as BS
import Base
import Uniq
import ByteStringUtil

data IWord = Nil
           | Snoc {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Char
                  !IWord
  deriving (Eq)

instance Show IWord where
  show = iword

-- | Ensure that hashing is O(1) by caching hashes at each step.
instance Hashable IWord where
  hashWithSalt s Nil = s
  hashWithSalt s (Snoc h _ _) = s + h * 31

  hash Nil = 0
  hash (Snoc h _ _) = h

w `snoc` c = Snoc (fromEnum c `hashWithSalt` w) c w

prefixes Nil = [Nil]
prefixes w@(Snoc _ _ rest) = w : prefixes rest

iword = reverse . toBackwardsList
  where toBackwardsList Nil = []
        toBackwardsList (Snoc _ c rest) = c : toBackwardsList rest

mkIWord bs = foldl' snoc Nil $ BS.unpack bs

type Dict = H.HashMap IWord (Bool, Bool)

search :: Dict -> RawBoard -> IPath -> IWord -> Results
search d b path word =
  let (complete, continue) = H.lookupDefault (False, False) word d
  in (if complete then ((BS.pack $ iword word, ipath b $ reverse path) :) else id)
     (if continue
        then [r | n <- neighborIndices b (head path)
                , n `notElem` path
                , let p' = n : path
                , let w' = word `snoc` (b `at` n)
                , r <- search d b p' w'
                ]
        else [])

prefixPattern = (True, False) : repeat (False, True)

solver :: Solver
solver d b =
  let cs = BS.pack $ sort $ ungrid b
      df = [mkIWord w | (w, sw) <- d, sw `isSubsequenceOf` cs]
      d' = H.fromListWith (\(a, b) (c, d) -> force (a || c, b || d)) $
           [e | w <- df, e <- zip (prefixes w) prefixPattern]
  in uniqBy fst $
     [r | pos <- indices b
        , r <- search d' b [pos] $ Nil `snoc` (b `at` pos)
        ]
