{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Tut.Json where

import           Tut.Config
import           Data.Aeson.Types
import           Data.Aeson
import           Tut.Misc
import qualified Data.Map         as M
import qualified Data.Text        as T

--TODO remove repetition
--TODO handle default config dependencies better
--TODO parse more configuration options, including flag for ghcicmd callback

parseInclude :: IncludeConfig -> Value -> Parser IncludeConfig
parseInclude d (String t) =
    return $
        d { includeFiles = M.singleton "" (T.unpack t) `M.union` includeFiles d
          }
parseInclude d x@(Object v) = do
    fs <- (parseJSON x)
    return $ d { includeFiles = fs }
parseInclude _ _ = empty

parseGhciConfig :: (FilePath -> GhciCmd)
                -> GhciConfig
                -> Value
                -> Parser GhciConfig
parseGhciConfig dcmd dcnfg x@(Object v) =
    v .:? "sessions" >>=
        \case
            Just m -> do
                d <- (fmap . fmap) promptDisplay (v .:? "prompt") .!=
                         display dcnfg
                withSessions m (dcnfg { display = d })
            Nothing -> withSessions x dcnfg
  where
    withSessions m d = do
        ss <- parseJSON m >>= traverse (parseGhciCmd dcmd)
        return $ d { sessions = ss `M.union` sessions d }
parseGhciConfig dcmd dcnfg (String t) =
    return $
        dcnfg { sessions = M.singleton "" (dcmd (T.unpack t))
                           `M.union`
                           sessions dcnfg
              }

parseGhciCmd :: (FilePath -> GhciCmd) -> Value -> Parser GhciCmd
parseGhciCmd d (Object v) = do
    dir <- v .: "dir"
    let gc = d dir
    cmd <- v .:? "cmd" .!= command gc
    return $ gc { command = cmd }
parseGhciCmd d (String t) =
    return $ d (T.unpack t)

parseTut :: IncludeConfig
         -> (FilePath -> GhciCmd)
         -> GhciConfig
         -> Object
         -> Parser (Maybe IncludeConfig, Maybe GhciConfig)
parseTut i gcm gcf v = do
    x <- (v .:? "include") >>= traverse (parseInclude i)
    y <- (v .:? "ghci") >>= traverse (parseGhciConfig gcm gcf)
    return (x, y)