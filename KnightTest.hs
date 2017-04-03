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

--  describe "Validate visitSquare function" $ do
--    it "visitSquare is supposed to add the square to the node" $ do
--      let node = (1, 0, [[True]], (1,1), [])
--      visitSquare node (1,1) `shouldBe` (1,1,[[False]],(1,1),[(1,1)])
--    it "visitSquare is supposed to add the square to the node" $ do
--      let node = (2, 0, [[True,True],[True,True]], (2,2), [])
--      visitSquare node (2,2) `shouldBe` (2,1,[[True,True],[True,False]],(2,2),[(2,2)])

  describe "Validate updateBoard function" $ do
      it "updateBoard is supposed to modify the board" $ do
        updateBoard [[True,True],[True,True]] (1,1) `shouldBe` [[False,True],[True,True]]
      it "updateBoard is supposed to modify the board" $ do
        updateBoard [[True,True,True],[True,True,True],[True,True,True]] (2,1) `shouldBe` [[True,True,True],[False,True,True],[True,True,True]]

  describe "Validate fullPath function" $ do
    it "fullPath is supposed to be FALSE" $ do
      fullPath (2, 0, [[True,True],[True,True]], (0,0), []) `shouldBe` False
    it "fullPath is supposed to be FALSE" $ do
      fullPath (2, 3, [[False,False],[False,True]], (0,0), []) `shouldBe` False
    it "fullPath is supposed to be TRUE" $ do
      fullPath (2, 4, [[False,False],[False,False]], (0,0), []) `shouldBe` True

  describe "Validate validJumps function" $ do
    it "validJumps is supposed to create 2 nodes" $ do
      let board = createBoard 3
      let node = (3, 1, board, (1,1), [(1,1)])
      let validJmps = validJumps node
      length validJmps `shouldBe` 2
    it "validJumps is supposed to create 8 nodes" $ do
      let board = createBoard 5
      let node = (5, 1, board, (3,3), [(3,3)])
      let validJmps = validJumps node
      length validJmps `shouldBe` 8
    it "validJumps is supposed to create 2 nodes" $ do
      let board = [[False,False,False],[False,False,False],[False,True,False]]
      let node = (3, 8, board, (1,1), [])
      let validJmps = validJumps node
      length validJmps `shouldBe` 1

  describe "Validate knightTravel function" $ do
      it "knightTravel is supposed to find the path for a 1 size board" $ do
        knightTravel 1 (1,1) `shouldBe` [(1,1)]
      it "knightTravel is supposed to find the path for a 2 size board" $ do
        knightTravel 2 (1,1) `shouldBe` []
      it "knightTravel is supposed to find the path for a 3 size board" $ do
        knightTravel 3 (1,1) `shouldBe` []
      it "knightTravel is supposed to find the path for a 4 size board" $ do
        knightTravel 4 (1,1) `shouldBe` []
      it "knightTravel is supposed to find the path for a 5 size board" $ do
        knightTravel 5 (1,1) `shouldBe` [(1,1),(2,3),(1,5),(3,4),(1,3),(2,1),(4,2),(5,4),(3,5),(1,4),(2,2),(4,1),(3,3),(2,5),(4,4),(5,2),(3,1),(1,2),(2,4),(4,5),(5,3),(3,2),(5,1),(4,3),(5,5)]
      it "knightTravel is supposed to find the path for a 6 size board" $ do
        knightTravel 6 (1,1) `shouldBe` [(1,1),(2,3),(1,5),(3,4),(1,3),(2,1),(3,3),(1,2),(2,4),(1,6),(3,5),(5,6),(6,4),(5,2),(3,1),(4,3),(6,2),(4,1),(2,2),(1,4),(2,6),(4,5),(6,6),(5,4),(4,2),(6,1),(5,3),(3,2),(5,1),(6,3),(5,5),(3,6),(4,4),(2,5),(4,6),(6,5)]
      it "knightTravel is supposed to find the path for a 7 size board" $ do
        knightTravel 7 (1,1) `shouldBe` [(1,1),(2,3),(1,5),(2,7),(3,5),(1,4),(2,2),(3,4),(1,3),(2,1),(3,3),(1,2),(2,4),(1,6),(3,7),(2,5),(1,7),(3,6),(4,4),(3,2),(5,1),(4,3),(3,1),(5,2),(6,4),(7,2),(5,3),(4,1),(6,2),(7,4),(5,5),(7,6),(5,7),(4,5),(2,6),(4,7),(6,6),(5,4),(4,6),(6,7),(7,5),(5,6),(7,7),(6,5),(7,3),(6,1),(4,2),(6,3),(7,1)]
      it "knightTravel is supposed to find the path for a 8 size board" $ do
        knightTravel 8 (1,1) `shouldBe` [(1,1),(2,3),(1,5),(2,7),(3,5),(1,4),(2,2),(3,4),(1,3),(2,1),(3,3),(1,2),(2,4),(1,6),(2,8),(3,6),(1,7),(2,5),(3,7),(1,8),(2,6),(3,8),(4,6),(5,4),(4,2),(6,1),(5,3),(3,2),(4,4),(5,2),(3,1),(4,3),(5,1),(7,2),(6,4),(4,5),(5,7),(7,8),(8,6),(6,5),(8,4),(7,6),(8,8),(6,7),(4,8),(5,6),(6,8),(4,7),(5,5),(7,4),(8,2),(6,3),(7,1),(8,3),(7,5),(8,7),(6,6),(5,8),(7,7),(8,5),(7,3),(8,1),(6,2),(4,1)]