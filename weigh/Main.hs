{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import Weigh

import Data.Typeable
import Control.DeepSeq (force, NFData)
import Bench
import Base

import qualified DP.TwoPass
import qualified DP.OnePass
import qualified DP.FilteredOnePass
import qualified Traversal.Trie
import qualified Traversal.Set
import qualified Traversal.FilteredSet
import qualified Traversal.FilteredHAMT
import qualified Traversal.FilteredTrie
import qualified Traversal.FilteredPrefixHAMT
import qualified Traversal.FilteredPrefixSet
import qualified Traversal.IncrementalFPHAMT

sfunc :: forall s. (Solver s, Typeable s)
      => RawDictionary -> Weigh ()
sfunc d = func name
               (\(cd, b) -> solve @s cd (cookBoard @s b))
               (force (d, board4x4))
  where name = tyConModule $ typeRepTyCon $ typeRep (Proxy @s)

main = do
  !d <- force <$> loadDictFile "bench/dict.txt"
  mainWith $ do
    setColumns [Case, Allocated, GCs, Live]

    io "dictionary" (fmap force . loadDictFile) "bench/dict.txt"

    sfunc @DP.TwoPass.T d
    sfunc @DP.OnePass.T d
    sfunc @DP.FilteredOnePass.T d
    sfunc @Traversal.Trie.T d
    sfunc @Traversal.Set.T d
    sfunc @Traversal.FilteredSet.T d
    sfunc @Traversal.FilteredHAMT.T d
    sfunc @Traversal.FilteredTrie.T d
    sfunc @Traversal.FilteredPrefixHAMT.T d
    sfunc @Traversal.FilteredPrefixSet.T d
    sfunc @Traversal.IncrementalFPHAMT.T d
