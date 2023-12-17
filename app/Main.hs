module Main (main) where

import Lib
import System.IO
import Control.Concurrent

main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  let n = 10
  mutex <- newEmptyMVar
  endFlags <- newChan
  forkIO $ threadSolve mutex endFlags 1 15 2 60 4
  forkIO $ threadSolve mutex endFlags 15 30 2 60 4
  forkIO $ threadSolve mutex endFlags 30 45 2 60 4
  forkIO $ threadSolve mutex endFlags 45 60 2 60 4
  putMVar mutex ()
  mapM_ (const $ readChan endFlags) [1..n] 
  
threadSolve :: MVar () -> Chan () -> Int -> Int -> Int -> Int -> Int -> IO()
threadSolve mutex endFlags from to base modulus remainder_ = do
  solution <- printSolution from to base modulus remainder_
  takeMVar mutex
  putStrLn solution
  putMVar mutex ()
  writeChan endFlags ()

printSolution :: Int -> Int -> Int -> Int -> Int -> IO String
printSolution from to base modulus remainder_ = do
  tid <- myThreadId
  let solved = batchSolve [from..to] base modulus remainder_
  let result = "Result " ++ show tid ++ " : " ++ show solved
  return $! result
