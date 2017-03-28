module Main where

import Knight

main :: IO ()
main = do
    putStrLn $ show (knightTravel 6 (1,1))
