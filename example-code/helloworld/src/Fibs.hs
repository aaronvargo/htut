module Fibs where

--label fibs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--label fibs1

fibs1 :: [Integer]
fibs1 = go 0 1
  where go a b = a : go b (a + b)
