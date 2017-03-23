module Main where

import Knight
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "Validate createBoard function" $ do
    it "createBoard is supposed to create a board of the size 1" $ do
      createBoard 1 `shouldBe` [[False]]
    it "createBoard is supposed to create a board of the size 2" $ do
      createBoard 2 `shouldBe` [[False,False],[False,False]]

  describe "Validate activateSquare function" $ do
      it "activateSquare is supposed to activate a square" $ do
        activateSquare [[False]] (0,0) `shouldBe` [[True]]
      it "activateSquare is supposed to activate a square" $ do
        activateSquare [[False,False],[False,False]] (1,1) `shouldBe` [[False,False],[False,True]]
      it "activateSquare is supposed to NOT activate a square (out of the board)" $ do
        activateSquare [[False,False],[False,False]] (1,2) `shouldBe` [[False,False],[False,False]]
