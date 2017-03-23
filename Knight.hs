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

--fullPath :: Node -> Bool
--fullPath node = false
-- reduce o fall left
-- Se trata de una función que comprueba si un nodo contiene, o no, un camino completo (que recorra todo el tablero).

--validJumps :: Node -> [Node]
--validJumps node =
-- Esta función recibe un nodo y devuelve la lista de compleciones de dicho nodo, esto es, todos aquellos nodos en los
-- que se visita una nueva casilla válida (que está dentro del tablero y que aún no ha sido visitada).

createFile :: Int -> File
createFile size = [False | _ <- [1..size]]

createBoard :: Int -> Board
createBoard size = [createFile size | _ <- [1..size]]

activateSquare :: Board -> Square -> Board
activateSquare board (x,y) =
    let file_to_modify = board !! x
        modified_file = updateFile file_to_modify y
    in  updateBoard board modified_file x

updateFile :: File -> Int -> File
updateFile  (f:fs) index
    | index == 0 = not f:fs
    | otherwise  = f : (updateFile fs (index-1))
updateFile [] index = []

updateBoard :: Board -> File -> Int -> Board
updateBoard  (b:bs) file index
    | index == 0 = file:bs
    | otherwise  = b : (updateBoard bs file (index-1))
updateBoard [] [] index = []

-- Main Functions

type Node = (Int,Int,Board,Square,Path)

--knightTravel :: Int -> Square -> Path
--knightTravel size square =
--    let board = createBoard size
--        path = []
--        node = (size, size, board, square, path)
--    in bt fullPath validJumps node

