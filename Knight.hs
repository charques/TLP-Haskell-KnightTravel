module Knight where

-- Data Types used in program

type File = [Bool]
type Board = [File]
type Square = (Int,Int)
type Path = [Square]

-- Backtracking scheme

bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt isSol comp node
    | isSol node = [node]
    | otherwise = concat (map (bt isSol comp) (comp node))
-- bt lo está aplicacion parcialmente (solo 2 params). Devuelve una funcion que espera un nodo (tercer parámetro)

-- Auxiliary Functions

-- Comprueba si un nodo contiene, o no, un camino completo.
fullPath :: Node -> Bool
--fullPath (_,_,board,_,_) = not (foldr (\x y -> (foldr (||) False x) || y) False board)
fullPath (size,pathLength,_,_,_) = (pathLength == size*size)
-- NOTA: El fold interno aplica la operación OR por columna (x). En la y va el resultado de la columna previamente reducida.

validJumps :: Node -> [Node]
validJumps (size,pathLength,board,square,path) =
    let validSqrs = validSquares square board
    in map (\x -> visitSquare (size,pathLength,board,square,path) x) validSqrs
-- Esta función recibe un nodo y devuelve la lista de compleciones de dicho nodo, esto es, todos aquellos nodos en los
-- que se visita una nueva casilla válida (que está dentro del tablero y que aún no ha sido visitada).

validSquares :: Square -> Board -> [Square]
validSquares (x,y) board =
    let possibleSquares = [(x+2, y+1), (x+1, y+2), (x-1, y+2), (x-2, y+1), (x-2, y-1), (x-1, y-2), (x+1, y-2), (x+2, y-1)]
    in filter (\square -> validSquare square board) possibleSquares

createBoard :: Int -> Board
createBoard size = [ [ True | _ <- [1..size] ] | _ <- [1..size] ]

validSquare :: Square -> Board -> Bool
validSquare (x,y) board =
    let size = length board
        boardValue = (board !! (x-1)) !! (y-1)
    in inBoard size (x,y) && boardValue

inBoard :: Int -> Square -> Bool
inBoard size (x,y) = (1<=x) && (x<=size) && (1<=y) && (y<=size)

visitSquare :: Node -> Square -> Node
visitSquare (size,pathLength,board,_,path) (x,y) =
    let modified_board = updateBoard board (x,y)
        modified_path = path ++ [(x,y)]
    in  (size, (pathLength+1), modified_board, (x,y), modified_path)

updateBoard :: Board -> Square -> Board
updateBoard board (x,y) =
    let file = board !! (x-1)
    in update board (x-1) (update file (y-1) False)

update :: [a] -> Int -> a -> [a]
update list n newElement =
    take n list ++ [newElement] ++ drop (n+1) list

getNodePath :: Node -> Path
getNodePath (_,_,_,_,path) = path

getFirstPath :: [Node] -> Path
getFirstPath nodes
    | (length nodes) == 0 = []
    | otherwise = getNodePath (nodes !! 0)

-- Main Functions

type Node = (Int,Int,Board,Square,Path)

knightTravel :: Int -> Square -> Path
knightTravel size square =
    let emptyNode = (size, 0, (createBoard size), square, [])
        firstNode = visitSquare emptyNode square
        result = bt fullPath validJumps firstNode
    in  getFirstPath(result)

