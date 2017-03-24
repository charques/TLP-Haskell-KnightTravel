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
  | otherwise  = concat (map (bt isSol comp) (comp node))
-- bt lo está aplicacion parcialmente (solo 2 params). Devuelve una funcion que espera un nodo (tercer parámetro)


-- Auxiliary Functions

-- Comprueba si un nodo contiene, o no, un camino completo.
fullPath :: Node -> Bool
fullPath (_,_,board,_,_) = not (foldr (\x y -> (foldr (||) False x) || y) False board)
-- NOTA: El fold interno aplica la operación OR por columna (x). En la y va el resultado de la columna previamente reducida.

--validJumps :: Node -> [Node]
--validJumps (x,y,board,square,path) =
--    let v = board
--    in
-- Esta función recibe un nodo y devuelve la lista de compleciones de dicho nodo, esto es, todos aquellos nodos en los
-- que se visita una nueva casilla válida (que está dentro del tablero y que aún no ha sido visitada).

createFile :: Int -> File
createFile size = [True | _ <- [1..size]]

createBoard :: Int -> Board
createBoard size = [createFile size | _ <- [1..size]]

visitSquare :: Board -> Square -> Board
visitSquare board (x,y) =
    let file_to_modify = board !! x
        modified_file = updateFile file_to_modify y
    in  updateBoard board modified_file x

updateFile :: File -> Int -> File
updateFile  (f:fs) index
    | index == 0 = not f:fs
    | otherwise  = f : (updateFile fs (index-1))
updateFile [] _ = []

updateBoard :: Board -> File -> Int -> Board
updateBoard  (b:bs) file index
    | index == 0 = file:bs
    | otherwise  = b : (updateBoard bs file (index-1))
updateBoard [] [] _ = []
updateBoard [] (_:_) _ = []

-- Main Functions

type Node = (Int,Int,Board,Square,Path)

--knightTravel :: Int -> Square -> Path
--knightTravel size square =
--    let board = createBoard size
--        path = []
--        node = (size, size, board, square, path)
--    in bt fullPath validJumps node

