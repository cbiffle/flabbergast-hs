import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.List.Split

import Base
import qualified B1

main :: IO ()
main = hspec $ do
  genericSpec "B1" unSmallBoard B1.solver

arbBoard :: Int -> Int -> Gen RawBoard
arbBoard mw mh = do
    w <- choose (1, mw)
    h <- choose (1, mh)
    cs <- replicateM (w * h) $ choose ('A', 'Z')
    pure $ chunksOf h cs

newtype SmallBoard = SmallBoard { unSmallBoard :: RawBoard }
  deriving (Eq, Show)

instance Arbitrary SmallBoard where
  arbitrary = SmallBoard <$> arbBoard 2 2

checkResult :: (String, Path) -> RawBoard -> Bool
checkResult (w, p) b = extracted == w
  where
    extracted = map (get b) p
    get b (x, y) = b !! x !! y

genericSpec :: (Arbitrary a, Show a)
            => String -> (a -> RawBoard) -> Solver -> Spec
genericSpec name getBoard solver = describe name $ do
  context "corner cases" $ do
    it "finds no solutions in an empty board" $ property $
      solver [] [[]] === []

    it "finds no solutions in any board if the dictionary is empty" $ property $
      \ab -> solver [] (getBoard ab) === []

  context "single word search" $ do
    let word = "ABCD"
        case_ boardf path =
            solver [word] (boardf word) `shouldBe` [(word, path)]
    it "finds word right" $
      case_ (:[]) [(x, 0) | x <- [0 .. length word - 1]]
    it "finds word left" $
      case_ ((:[]) . reverse) [(x, 0) | x <- reverse [0 .. length word - 1]]
    it "finds word down" $
      case_ (chunksOf 1) [(0, y) | y <- [0 .. length word - 1]]
    it "finds word up" $
      case_ (reverse . chunksOf 1)
            [(0, y) | y <- reverse [0 .. length word - 1]]

  context "single path in board" $ do
    let word = "ABCD"
        board = [ "AB-"
                , "-C-"
                , "D--"
                ]
        path = [(0,0), (1,0), (1,1), (0,2)]
    it "finds it" $
      solver [word] board `shouldBe` [(word, path)]
    it "finds it reversed" $
      solver [reverse word] board `shouldBe` [(reverse word, reverse path)]
    it "finds both" $
      solver [word, reverse word] board
        `shouldBe` [(word, path), (reverse word, reverse path)]
