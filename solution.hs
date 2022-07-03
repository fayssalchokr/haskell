-- Enter your code here. Read input from STDIN. Print output to STDOUT
-- Programmed By Fayssal Chokri
-- MIT Licence
module Main where

import Control.Monad (replicateM)


data Person = DEAD | CENTRAL | LEFT | RIGHT

instance Show Person where
  show p = case p of  DEAD -> "DEAD"
                      CENTRAL -> "CENTRAL"
                      LEFT -> "LEFT"
                      RIGHT -> "RIGHT"


isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all id [ n `mod` k /= 0| k <- [2..isqrt n]]


containsZero :: Int -> Bool
containsZero n = '0' `elem` show n

directionPrime :: (Int -> String -> String) -> Int -> Bool
directionPrime f n = all id [isPrime k | i <- [1..l-1],
                                  let k = read (f i s) :: Int]
  where s = show n
        l = length s



rightPrime :: Int -> Bool
rightPrime n = directionPrime take n


leftPrime :: Int -> Bool
leftPrime n = directionPrime drop n

centralPrime :: Int -> Bool
centralPrime n = rightPrime n && leftPrime n


checkId :: Int -> Person
checkId n = state
  where zero = containsZero n
        prime = isPrime n
        left = leftPrime n
        right = rightPrime n
        state
          | zero || not prime = DEAD
          | left && right = CENTRAL
          | left = LEFT
          | right = RIGHT
          | otherwise = DEAD


main :: IO()
main = do n <- readLn
          nums <- replicateM n readLn
          mapM_ print $ map checkId nums
