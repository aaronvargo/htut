Hello
=====

hello

``` {.haskell}
1 + 2
```

``` {.haskell}
module Main where
```

``` {.haskell}
main :: IO ()
main = do
  putStrLn "hello world"
```

Fibonacci sequence:

``` {.haskell}
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

The first 10 fibonacci numbers:

``` {.haskell}
λ> take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
```