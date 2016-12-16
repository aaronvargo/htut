module Main where

import Fibs

--label main

main :: IO ()
main = do
  putStrLn "Hello, world!"
  putStrLn $ "The 112th fibonacci number is " ++ show (fibs !! 112)
