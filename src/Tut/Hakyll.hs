{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tut.Hakyll where

import Hakyll
import qualified Data.Text as T
import Control.Monad.Base
import Tut.Include
import Tut.Ghci
import Tut
import Control.Monad.Except
import Data.Foldable
import Text.Pandoc
import Tut.Json

newtype IOCompiler a = IOCompiler
  { runIOCompiler :: Compiler a
  } deriving (Functor, Applicative, Monad)

instance MonadBase IO IOCompiler where
  liftBase = IOCompiler . unsafeCompiler

instance MonadLoadFile IOCompiler where
  loadFile = IOCompiler . fmap (T.pack . itemBody) . load . fromFilePath

instance MonadError String IOCompiler where
  throwError = IOCompiler . throwError . (:[])
  catchError (IOCompiler c) f = IOCompiler $ catchError c (runIOCompiler . f . fold)

hakyllTut
  :: Walkable Block b
  => MetaConfig GhciConfig -> MetaConfig IncludeConfig -> b -> Compiler b
hakyllTut gcfg icfg doc = do
  meta <- getMetadata =<< getUnderlying
  runIOCompiler $
    doTransformation' (tutTransformation leakyAllocator gcfg icfg) meta doc

defaultHakyllTut :: Walkable Block b => b -> Compiler b
defaultHakyllTut = hakyllTut ghciMetaConfig includeMetaConfig
