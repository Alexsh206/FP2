{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import System.IO

type Matrix = [[Double]]   
type Vector = [Double]



forwardElim :: Matrix -> Matrix
forwardElim mat = foldl elimStep mat [0 .. n - 1]
  where
    n = length mat

    elimStep :: Matrix -> Int -> Matrix
    elimStep m k =
      let (upper, rest) = splitAt (k + 1) m
          pivotRow      = m !! k
          newRest       = parMap rdeepseq (elimRow pivotRow k) rest
       in upper ++ newRest

    elimRow :: [Double] -> Int -> [Double] -> [Double]
    elimRow pivotRow k row =
      let pivot = pivotRow !! k
          factor = (row !! k) / pivot
          newRow = zipWith (\a b -> a - factor * b) row pivotRow
       in newRow


backSub :: Matrix -> Vector
backSub mat = solve (replicate n 0) (n - 1)
  where
    n = length mat

    solve xs i
      | i < 0     = xs
      | otherwise =
          let row     = mat !! i
              aii     = row !! i
              rhs     = last row
              sumRest = sum [ (row !! j) * (xs !! j) | j <- [i+1 .. n-1] ]
              xi      = (rhs - sumRest) / aii
              xs'     = take i xs ++ [xi] ++ drop (i+1) xs
           in solve xs' (i - 1)


gaussSolve :: Matrix -> Vector
gaussSolve = backSub . forwardElim


testMatrix :: Matrix
testMatrix =
  [ [ 2,  1, -1,  8 ]
  , [-3, -1,  2, -11]
  , [-2,  1,  2, -3]
  ]


readMatrix :: IO Matrix
readMatrix = do
  putStrLn "Enter number of variables n:"
  n <- readLn
  putStrLn $ "Enter " ++ show n ++ " rows of augmented matrix (n+1 numbers per row):"
  mapM (\i -> do
          putStrLn $ "Row " ++ show i ++ ":"
          line <- getLine
          pure (map read (words line))
       ) [1..n]


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "=== Parallel Gaussian Elimination (Haskell, threaded) ==="
  putStrLn "1. Use built-in test system (3x3)"
  putStrLn "2. Enter your own system"
  putStrLn "Choice:"
  c <- getLine
  mat <- case c of
    "1" -> pure testMatrix
    "2" -> readMatrix
    _   -> do
      putStrLn "Invalid choice, using test system."
      pure testMatrix

  putStrLn "\nAugmented matrix:"
  mapM_ print mat

  let solution = gaussSolve mat
  putStrLn "\nSolution vector x:"
  print solution
