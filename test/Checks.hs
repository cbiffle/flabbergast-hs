{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Checks (genericSpec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Arrow ((&&&))
import Control.Monad
import Data.List.Split
import qualified Data.ByteString.Char8 as BS

import Base

mkDict :: [BS.ByteString] -> RawDictionary
mkDict = fmap (id &&& BS.sort)

genericSpec :: forall s. (Solver s) => Int -> Spec
genericSpec n = do
  let bprop :: (Testable p) => p -> Property
      bprop = mapSize (const n) . property
      solver = cookAndSolve @s

  context "corner cases" $ do
    it "finds no solutions in an empty board" $ property $
      solver [] (mkGridOf [""]) === []

    it "finds no solutions in any board if the dictionary is empty" $ bprop $
      \(SizedBoggle92 b) -> solver [] b === []

  context "triviality" $ do
    context "single word" $ do
      let word = "ABCD"
          -- We're going to generate boards containing only that word, each one
          -- derived by boardf.
          case_ boardf path = do
              let board = mkGridOf $ boardf word
                  dict = mkDict [word]
              let play = solver dict board
              checkPlay dict board play `shouldBe` Right ()
              play `shouldBe` [(word, path)]
      it "finds word right" $
        case_ (:[]) 
              [(x, 0) | x <- [0 .. BS.length word - 1]]
      it "finds word left" $
        case_ ((:[]) . BS.reverse)
              [(x, 0) | x <- reverse [0 .. BS.length word - 1]]
      it "finds word down" $
        case_ (map BS.pack . chunksOf 1 . BS.unpack)
              [(0, y) | y <- [0 .. BS.length word - 1]]
      it "finds word up" $
        case_ (map BS.pack . reverse . chunksOf 1 . BS.unpack)
              [(0, y) | y <- reverse [0 .. BS.length word - 1]]

    context "single 2D path" $ do
      let word = "ABCD"
          board = mkGridOf [ "ABZ"
                           , "ZCZ"
                           , "DZZ"
                           ]
          path = [(0,0), (1,0), (1,1), (0,2)]
      it "finds forward" $
        solver (mkDict [word]) board `shouldBe` [(word, path)]
      it "finds backwards" $
        solver (mkDict [BS.reverse word]) board
          `shouldBe` [(BS.reverse word, reverse path)]
      it "finds both" $
        solver (mkDict [word, BS.reverse word]) board
          `shouldBe` [(word, path), (BS.reverse word, reverse path)]

  context "rules" $ modifyMaxSuccess (const 25) $ do
    it "makes legal plays" $ bprop $ \(SizedBoggle92 b) (FakeDict d) ->
      checkPlay d b (solver d b) `shouldBe` Right ()
