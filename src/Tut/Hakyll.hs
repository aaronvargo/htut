{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Tut.Hakyll
    ( hakyllTut
    , defaultHakyllTut
    ) where

import           Data.Aeson.Types
import           Hakyll
import           Text.Pandoc
import           Tut.Config
import           Tut.Pandoc
import           Tut.Json
import qualified Data.Text           as T
import           Tut.Misc
import           Control.Arrow
import           Control.Applicative

instance MonadLoadFile Compiler where
    loadFile = fmap (T.pack . itemBody) . load . fromFilePath

newtype IOCompiler m a = IOCompiler { runCompiler :: m a }
    deriving (Functor, Applicative, Monad, MonadMetadata)

deriving instance MonadError e m => MonadError e (IOCompiler m)

instance MonadTrans IOCompiler where
    lift = IOCompiler

instance MonadIO (IOCompiler Compiler) where
    liftIO = IOCompiler . unsafeCompiler

hakyllTut :: IncludeConfig
          -> (FilePath -> GhciCmd)
          -> GhciConfig
          -> Pandoc
          -> Compiler Pandoc
hakyllTut i gc gf doc = do
    m <- getMetadata =<< getUnderlying
    cfg <- eitherError . left (: []) $ parseEither (parseTut i gc gf) m
    runCompiler . collapseErrors err $ tut cfg doc
  where
    err = \case
        UnsupportedFileExt s -> [ "Unsupported extension: " ++ s ]
        BlockNotFound fp lbl -> [ "Block '" ++ lbl ++ "' not found in: " ++ fp ]
        IncludeNotFound s -> [ "No included file with name: " ++ s ]
        SessionNotFound s -> [ "No ghci session with name: " ++ s ]

defaultHakyllTut :: Pandoc -> Compiler Pandoc
defaultHakyllTut = hakyllTut defaultIncludeConfig defaultGhciCmd defaultGhciConfig
