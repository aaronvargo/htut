{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Tut
import Tut.Pandoc
import Tut.Misc

import Tut.Json
import Tut.Repl

import System.Directory
import System.FilePath

myFileTransform
  :: ( MonadError e m
     , AsUndefinedField e
     , MonadResource m
     , AsTransformationError e
     , AsReplError e
     , IncludeError e
     , MonadLoadFile m
     , AsPandocError e
     )
  => FilePath -> FilePath -> m ()
myFileTransform inp out = transformMarkdownFile inp out def def defaultTutTransformation

myIOFileTransform :: FilePath -> FilePath -> IO ()
myIOFileTransform inp out = handleErrors (putStrLn . mappend "Error: ") . runResourceT $ myFileTransform inp out

main :: IO ()
main = do
  let inp = "htut-input"
  d <- getCurrentDirectory
  pths <- listDirectory (combine d inp)
  let f pth = do
        putStrLn $ "Generating " ++ pth
        myIOFileTransform (d </> inp </> pth) (d </> pth)
  mapM_ f pths
