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

--  describe "Validate inBoard function" $ do
--    it "inBoard is supposed to be TRUE" $ do
--      inBoard 4 (1,1) `shouldBe` True
--    it "inBoard is supposed to be TRUE" $ do
--      inBoard 4 (4,4) `shouldBe` True
--    it "inBoard is supposed to be FALSE" $ do
--      inBoard 4 (0,0) `shouldBe` False
--    it "inBoard is supposed to be FALSE" $ do
--      inBoard 4 (5,5) `shouldBe` False

--  describe "Validate validSquare function" $ do
--    it "validSquare is supposed to be TRUE" $ do
--      validSquare (1,1) [[True,False],[False,False]] `shouldBe` True
--    it "validSquare is supposed to be TRUE" $ do
--      validSquare (2,2) [[False,False],[False,True]]`shouldBe` True
--    it "validSquare is supposed to be FALSE" $ do
--      validSquare (1,1) [[False,True],[True,True]] `shouldBe` False
--    it "validSquare is supposed to be FALSE" $ do
--      validSquare (0,0) [[True,True],[True,True]] `shouldBe` False

--  describe "Validate validSquares function" $ do
--    it "validSquares is supposed to be [(3,2),(2,3)]" $ do
--      let board = createBoard 3
--      validSquares (1,1) board `shouldBe` [(3,2),(2,3)]
--   it "validSquares is supposed to be TRUE" $ do
--      let board = createBoard 5
--      validSquares (3,3) board `shouldBe` [(5,4),(4,5),(2,5),(1,4),(1,2),(2,1),(4,1),(5,2)]
--    it "validSquares is supposed to be TRUE" $ do
--        let board = [[False,False,False],[False,False,False],[False,True,False]]
--        validSquares (1,1) board `shouldBe` [(3,2)]

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
        knightTravel 5 (1,1) `shouldBe` [(1,1),(3,2),(5,3),(4,5),(2,4),(1,2),(3,3),(4,1),(2,2),(1,4),(3,5),(5,4),(4,2),(2,1),(1,3),(2,5),(4,4),(5,2),(3,1),(2,3),(1,5),(3,4),(5,5),(4,3),(5,1)]
      --it "knightTravel is supposed to find the path for a 5 size board" $ do
        --knightTravel 6 (1,1) `shouldBe` [(1,1),(3,2),(5,3),(4,5),(2,4),(1,2),(3,3),(4,1),(2,2),(1,4),(3,5),(5,4),(4,2),(2,1),(1,3),(2,5),(4,4),(5,2),(3,1),(2,3),(1,5),(3,4),(5,5),(4,3),(5,1)]