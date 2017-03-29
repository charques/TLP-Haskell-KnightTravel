module Knight where

--import Debug.Trace

-- Data Types used in program

type File = [Bool]
type Board = [File]
type Square = (Int,Int)
type Path = [Square]
type Node = (Int,Int,Board,Square,Path)

-- Backtracking scheme

bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt isSol comp node
    | isSol node = [node]
    | otherwise = concat (map (bt isSol comp) (comp node))

-- Auxiliary Functions

-- Checks if a node has a full path
fullPath :: Node -> Bool
fullPath (size,pathLength,_,_,_) = (pathLength == size*size)

-- Gets the list of possible next nodes and execute the next movement from the node passed as parameter
validJumps :: Node -> [Node]
--validJumps (size,pathLength,board,square,path) | trace ("validJumps " ++ show square) False = undefined
validJumps (size,pathLength,board,square,path) =
    --trace ("validJumps = " ++ show square) (f x)
    let validSqrs = validSquares square board
    in map (\x -> visitSquare (size,pathLength,board,square,path) x) validSqrs

-- Gets the list of possible next nodes from the current position in the board.
validSquares :: Square -> Board -> [Square]
validSquares (x,y) board =
    let possibleSquares = [(x+2, y+1), (x+1, y+2), (x-1, y+2), (x-2, y+1), (x-2, y-1), (x-1, y-2), (x+1, y-2), (x+2, y-1)]
    in filter (\square -> validSquare square board) possibleSquares

-- Checks if a square is in the board and is not already visited
validSquare :: Square -> Board -> Bool
validSquare (x,y) board =
    let size = length board
        boardValue = (board !! (x-1)) !! (y-1)
    in inBoard size (x,y) && boardValue

-- Checks if a square is in the board
inBoard :: Int -> Square -> Bool
inBoard size (x,y) = (1<=x) && (x<=size) && (1<=y) && (y<=size)

-- Adds a new square to the path saved in a node. Updates the board and the path.
visitSquare :: Node -> Square -> Node
visitSquare (size,pathLength,board,_,path) (x,y) =
    let modified_board = updateBoard board (x,y)
        modified_path = path ++ [(x,y)]
    in  (size, (pathLength+1), modified_board, (x,y), modified_path)

-- Marks a board position as visited
updateBoard :: Board -> Square -> Board
updateBoard board (x,y) =
    let file = board !! (x-1)
    in updateList board (x-1) (updateList file (y-1) False)

-- Updates a list passing the position to modify and the new element
updateList :: [a] -> Int -> a -> [a]
updateList list pos newElement =
    take pos list ++ [newElement] ++ drop (pos+1) list

-- Creates a board using the size especified as parameter
createBoard :: Int -> Board
createBoard size = [ [ True | _ <- [1..size] ] | _ <- [1..size] ]

-- Gets the first node path in al list of results
getFirstPath :: [Node] -> Path
getFirstPath nodes
    | (length nodes) == 0 = []
    | otherwise = getNodePath (nodes !! 0)

-- Retrieves the path of a node
getNodePath :: Node -> Path
getNodePath (_,_,_,_,path) = path

-- Main Functions

knightTravel :: Int -> Square -> Path
knightTravel size square =
    let emptyNode = (size, 0, (createBoard size), square, [])
        firstNode = visitSquare emptyNode square
        result = bt fullPath validJumps firstNode
    in  getFirstPath(result)

