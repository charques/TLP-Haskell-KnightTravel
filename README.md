# TLP-Haskell-KnightTravel
Prácticas Haskell - UNED Teoría de los Lenguajes de Programación 2016-2017

Iniciar el GHCI y carga del módulo. 

```haskell
ghci
:load knight-travel
```
Recargar los módulos cargados previamente.
```haskell
:reload
```

###### PASOS
0. cabal init
00. cabal update && cabal install hspec

cabal configure
cabal build
cabal install
cabal test

https://github.com/hspec/hspec


1. inicializacion de board: createFile, createBoard

Primera aproximación a fullPath
fullPath :: Node -> Bool
fullPath (_,_,board,_,_) = not (foldr (\x y -> (foldr (||) False x) || y) False board)
-- NOTA: El fold interno aplica la operación OR por columna (x). En la y va el resultado de la columna previamente reducida.

Segunda aproximación a fullPath
fullPath (size,pathLength,_,_,_) = (pathLength == size*size)

Primera aproximación a create board
createFile :: Int -> File
createFile size = [True | _ <- [1..size]]

createBoard :: Int -> Board
createBoard size = [createFile size | _ <- [1..size]]

Segunda aproximación a create Board
createBoard size = [ [ True | _ <- [1..size] ] | _ <- [1..size] ]

Primera aproximación updateBoard
--updateFile :: File -> Int -> File
--updateFile  (f:fs) index
--    | index == 0 = not f:fs
--    | otherwise  = f : (updateFile fs (index-1))
--updateFile [] _ = []

--updateBoard :: Board -> File -> Int -> Board
--updateBoard  (b:bs) file index
--    | index == 0 = file:bs
--    | otherwise  = b : (updateBoard bs file (index-1))
--updateBoard [] [] _ = []
--updateBoard [] (_:_) _ = []

segunda aproximación updateboard
updateBoard :: Board -> Square -> Board
updateBoard board (x,y) =
    let file = board !! (x-1)
    in update board (x-1) (update file (y-1) False)
    
update :: [a] -> Int -> a -> [a]
update list n newElement =
    take n list ++ [newElement] ++ drop (n+1) list

2. activar una posición del tablero: activateSquare