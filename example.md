Hello, World!
=============

This is an example of a markdown document generated with htut, which
documents `example-code/helloworld`.

Here's the header of the Main module, which contains everything before
the first label:

``` {.haskell}
module Main where

import Fibs
```

Here's the main function:

``` {.haskell}
main :: IO ()
main = do
  putStrLn "Hello, world!"
  putStrLn $ "The 112th fibonacci number is " ++ show (fibs !! 112)
```

Let's run it:

``` {.haskell}
λ> main
Hello, world!
The 112th fibonacci number is 114059301025943970552219
```

Here's the implementation of fibs, from the fibs module:

``` {.haskell}
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

Let's get the first 10 fibonacci numbers:

``` {.haskell}
λ> take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
```

Here's another implementation, which is possibly easier to understand:

``` {.haskell}
λ> let fibs = go 0 1
λ|     go a b = a : go b (a + b)
λ| in take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
```

Multiline input is buggy, but at least that worked...