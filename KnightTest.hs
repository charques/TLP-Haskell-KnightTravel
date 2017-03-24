module Main where

import Knight
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "Validate createBoard function" $ do
    it "createBoard is supposed to create a board of the size 1" $ do
      createBoard 1 `shouldBe` [[True]]
    it "createBoard is supposed to create a board of the size 2" $ do
      createBoard 2 `shouldBe` [[True,True],[True,True]]

  describe "Validate visitSquare function" $ do
    it "visitSquare is supposed to activate a square" $ do
      visitSquare [[True]] (0,0) `shouldBe` [[False]]
    it "visitSquare is supposed to activate a square" $ do
      visitSquare [[True,True],[True,True]] (1,1) `shouldBe` [[True,True],[True,False]]
    it "visitSquare is supposed to NOT activate a square (out of the board)" $ do
      visitSquare [[True,True],[True,True]] (1,2) `shouldBe` [[True,True],[True,True]]

  describe "Validate fullPath function" $ do
      it "fullPath is supposed to be FALSE" $ do
        fullPath (0, 0, [[True,True],[True,True]], (0,0), []) `shouldBe` False
      it "fullPath is supposed to be FALSE" $ do
        fullPath (0, 0, [[False,False],[False,True]], (0,0), []) `shouldBe` False
      it "fullPath is supposed to be TRUE" $ do
        fullPath (0, 0, [[False,False],[False,False]], (0,0), []) `shouldBe` True