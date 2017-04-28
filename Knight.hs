module Knight where

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
validJumps (size,pathLength,board,(a,b),path) =
     [ let newPos = (a + x, b + y)
       in (size, (pathLength + 1), updateBoard board newPos, newPos, path ++ [newPos])
        | x <- [-2, -1, 1, 2],
          y <- [-2, -1, 1, 2],
          x /= y,
          x /= -y,
          x + a > 0,
          x + a <= size,
          y + b > 0,
          y + b <= size,
          isVisited board (a + x, b + y)
        ]

-- Check if a board position is visited
isVisited :: Board -> Square -> Bool
isVisited board (x,y) = (board !! (x - 1)) !! (y - 1)

-- Marks a board position as visited
updateBoard :: Board -> Square -> Board
updateBoard board (x,y) =
    let file = board !! (x - 1)
    in updateList board (x - 1) (updateList file (y - 1) False)

-- Updates a list passing the position to modify and the new element
updateList :: [a] -> Int -> a -> [a]
updateList list pos newElement =
    take pos list ++ [newElement] ++ drop (pos + 1) list

-- Creates a board using the size specified as parameter
createBoard :: Int -> Board
createBoard size = [ [ True | _ <- [1..size] ] | _ <- [1..size] ]

-- Gets the first node path in a list of results
getFirstPath :: [Node] -> Path
getFirstPath nodes =
    let (_,_,_,_,path) = if nodes /= [] then head nodes else (0, 0, [], (0,0), [])
    in path

-- Main Function

knightTravel :: Int -> Square -> Path
knightTravel size square =
    let board = updateBoard (createBoard size) square
        result = bt fullPath validJumps (size, 1, board, square, [square])
    in getFirstPath result