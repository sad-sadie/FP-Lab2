module Lib
    ( 
    batchSolve
    ) where

singleSolve :: Int -> Int -> Int -> Int -> Bool
singleSolve candidate base mod_ res = base ^ candidate `mod` mod_ == res

batchSolve :: [Int] -> Int -> Int -> Int -> [Int]
batchSolve candidates base mod_ res = [x | x <- candidates, singleSolve x base mod_ res]
    
