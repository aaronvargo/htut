module Main where

--label main

main :: IO ()
main = do
  putStrLn "hello world"

--label fibs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)