module Main where

import Knight

main :: IO ()
main = do
    putStrLn $ show (knightTravel 7 (1,1))
