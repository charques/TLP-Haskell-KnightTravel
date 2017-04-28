module Main where

import Knight

main :: IO ()
main = do
    putStrLn $ show (knightTravel 1 (1,1))
