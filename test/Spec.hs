{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad
import Data.List.Split
import Data.Typeable

import Base

import qualified Traversal.List
import qualified Traversal.List2
import qualified Traversal.Set
import qualified Traversal.SetPath
import qualified Traversal.FilteredSetPath
import qualified Traversal.FilteredByteStringSet
import qualified Traversal.FilteredPrefixSet
import qualified Traversal.FilteredPrefixSetVec
import qualified Traversal.Trie
import qualified Traversal.FilteredTrie
import qualified Traversal.FilteredPrefixHAMT
import qualified Traversal.FilteredPrefixHAMTVec

import qualified DP.TwoPass
import qualified DP.OnePass
import qualified DP.OnePassSinglePath
import qualified DP.OnePassSinglePathVec
import qualified DP.Filtered1PV

main :: IO ()
main = hspec $ do
  let slow = 2
      faster = 4
  genericSpec @Traversal.List.T                   slow
  genericSpec @Traversal.List2.T                  slow
  genericSpec @Traversal.Set.T                    slow
  genericSpec @Traversal.SetPath.T                slow
  genericSpec @Traversal.FilteredSetPath.T        slow
  genericSpec @Traversal.FilteredByteStringSet.T  slow
  genericSpec @Traversal.FilteredPrefixSet.T      faster
  genericSpec @Traversal.FilteredPrefixSetVec.T   faster
  genericSpec @Traversal.Trie.T                   faster
  genericSpec @Traversal.FilteredTrie.T           faster
  genericSpec @Traversal.FilteredPrefixHAMT.T     faster
  genericSpec @Traversal.FilteredPrefixHAMTVec.T  faster

  genericSpec @DP.TwoPass.T                       faster
  genericSpec @DP.OnePass.T                       faster
  genericSpec @DP.OnePassSinglePath.T             faster
  genericSpec @DP.OnePassSinglePathVec.T          faster
  genericSpec @DP.Filtered1PV.T                   faster

testName :: forall s. (Typeable s) => String
testName = tyConModule $ typeRepTyCon $ typeRep $ Proxy @s

genericSpec :: forall s. (Typeable s, Solver s) => Int -> Spec
genericSpec n = describe (testName @s) $ do
  let bprop :: (Testable p) => p -> Property
      bprop = mapSize (const n) . property
      solver = cookAndSolve @s

  context "corner cases" $ do
    it "finds no solutions in an empty board" $ property $
      solver [] [[]] === []

    it "finds no solutions in any board if the dictionary is empty" $ bprop $
      \(SizedBoggle92 b) -> solver [] b === []

  context "triviality" $ do
    context "single word" $ do
      let word = "ABCD"
          -- We're going to generate boards containing only that word, each one
          -- derived by boardf.
          case_ boardf path = do
              let board = boardf word
                  dict = [word]
              let play = solver dict board
              checkPlay dict board play `shouldBe` Right ()
              play `shouldBe` [(word, path)]
      it "finds word right" $
        case_ (:[]) 
              [(x, 0) | x <- [0 .. length word - 1]]
      it "finds word left" $
        case_ ((:[]) . reverse)
              [(x, 0) | x <- reverse [0 .. length word - 1]]
      it "finds word down" $
        case_ (chunksOf 1)
              [(0, y) | y <- [0 .. length word - 1]]
      it "finds word up" $
        case_ (reverse . chunksOf 1)
              [(0, y) | y <- reverse [0 .. length word - 1]]

    context "single 2D path" $ do
      let word = "ABCD"
          board = [ "ABZ"
                  , "ZCZ"
                  , "DZZ"
                  ]
          path = [(0,0), (1,0), (1,1), (0,2)]
      it "finds forward" $
        solver [word] board `shouldBe` [(word, path)]
      it "finds backwards" $
        solver [reverse word] board `shouldBe` [(reverse word, reverse path)]
      it "finds both" $
        solver [word, reverse word] board
          `shouldBe` [(word, path), (reverse word, reverse path)]

  context "rules" $ modifyMaxSuccess (const 25) $ do
    it "makes legal plays" $ bprop $ \(SizedBoggle92 b) (FakeDict d) ->
      checkPlay d b (solver d b) `shouldBe` Right ()
