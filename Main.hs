module Main where

import Knight

main :: IO ()
main = do
    putStrLn $ show (knightTravel 5 (1,1))
