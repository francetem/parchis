-- file Spec.hs

import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "starting player" $ do
    it "returns Red as starting player" $
      startingPlayer 1 `shouldBe` Green

  describe "board" $ do
    it "changes after move" $
      move (move initialBoard Red 0 1) Red 0 1 `shouldBe` Board (1, 1, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0)

    it "changes after move" $
      move (Board (72,72,0,0) (72,72,31,34) (72,72,0,0) (72,72,0,0)) Green 34 39 `shouldBe` Board (72, 72, 0, 0) (72, 72, 31, 39) (72, 72, 0, 0) (72, 72, 0, 0)

    it "returns right home number of pieces" $
      home Red (move (move initialBoard Red 0 1) Red 0 1) `shouldBe` 2

    it "returns number of pieces in square" $
      piecesIn (move (move (move initialBoard Green 0 3) Green 0 3) Green 0 1)  3 `shouldBe` 2

    it "kills a piece when another goes to the same position" $
      move (Board (2, 0, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0) (18, 0, 0, 0)) Blue 18 19 `shouldBe` Board (0, 0, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0) (19, 0, 0, 0)

    it "doesn't kill when is a safe position" $
      move (Board (8, 0, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0) (18, 0, 0, 0)) Blue 18 25 `shouldBe` Board (8, 0, 0, 0) (0, 0, 0, 0) (0, 0, 0, 0) (25, 0, 0, 0)


  describe "valid moves" $ do
    it "returns empty when not a five in initial" $
      moves Green initialBoard 6 3 `shouldBe` []

    it "returns initial move when there is a five in initial" $
      moves Green initialBoard 5 3 `shouldBe` [Movement 0 1]

    it "doesn't allow initial move when there are two pieces in the initial position" $
      moves Green (move (move initialBoard Green 0 1) Green 0 1) 5 3 `shouldBe` [Movement 1 6, Movement 1 4, Movement 1 9]

    it "it doesn't move to a square with two pieces" $
      moves Green (move (move (move initialBoard Green 0 4) Green 0 4) Green 0 1) 5 3 `shouldBe` [Movement 0 1, Movement 4 7, Movement 4 12]
