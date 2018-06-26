module Traversal.Generic where

import Base
import qualified Data.ByteString.Char8 as BS

type Trav w p r = w -> p -> [r] -> [r]

-- | Generates paths across 'b', directing the traversal with 'f'.
paths :: Trav BS.ByteString IPath (BS.ByteString, IPath)
      -> RawBoard
      -> Results
paths f b = [(word, ipath b path) | (p, c) <- zip [0..] (ungrid b)
                                  , (word, path) <- search [p] $ BS.singleton c]
  where
    search path word = f word path
                         [r | n <- neighborIndices b (head path)
                            , n `notElem` path
                            , r <- search (n : path)
                                          (word `BS.snoc` b `at` n)
                            ]
{-# INLINE paths #-}

-- | Uses a prefix predicate to prune paths when no words will be discovered by
-- continuing.
prefix :: (w -> Bool)   -- ^ Do any valid words start with 'w'?
       -> (w -> Bool)   -- ^ Is 'w' a complete valid word?
       -> Trav w [p] (w, [p])
prefix possible complete = \word path descend ->
  (if complete word then ((word, path) :) else id)
  (if possible word then descend else [])
{-# INLINE prefix #-}

-- | Performs an exhaustive check of all possible words against a dictionary
-- predicate. This is equivalent to (and implemented as) a prefix-pruned
-- traversal where the prefix predicate always insists more words are possible.
exhaustive :: (w -> Bool)
           -> Trav w [p] (w, [p])
exhaustive = prefix (const True)
{-# INLINE exhaustive #-}
