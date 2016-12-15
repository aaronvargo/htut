{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tut.Hakyll where

import Hakyll
import qualified Hakyll.Internal as I
import qualified Data.Text as T
import Control.Monad.Base
import Control.Monad.Trans.Control
import Tut.Include
import Tut.Ghci
import Tut
import Control.Monad.Except
import Data.Foldable
import Text.Pandoc
import Control.Monad.Trans.Resource
import Tut.Json

newtype IOCompiler a = IOCompiler
  { runIOCompiler :: Compiler a
  } deriving (Functor, Applicative, Monad)

instance MonadBase IO IOCompiler where
  liftBase = IOCompiler . unsafeCompiler

instance MonadIO IOCompiler where
  liftIO = liftBase

instance MonadLoadFile IOCompiler where
  loadFile = IOCompiler . fmap (T.pack . itemBody) . load . fromFilePath

instance MonadError String IOCompiler where
  throwError = IOCompiler . throwError . (:[])
  catchError (IOCompiler c) f = IOCompiler $ catchError c (runIOCompiler . f . fold)

instance MonadThrow IOCompiler where
  throwM = liftBase . throwM

instance MonadBaseControl IO IOCompiler where
  type StM IOCompiler a = I.CompilerResult a
  liftBaseWith :: ((forall α. IOCompiler α -> IO (I.CompilerResult α)) -> IO a)
               -> IOCompiler a
  liftBaseWith f = do
    cr <- IOCompiler $ I.compilerAsk
    liftBase $ f $ \(IOCompiler (I.Compiler g)) -> g cr
  restoreM = IOCompiler . I.Compiler . return . return

hakyllTut
  :: Walkable Block b
  => MetaConfig GhciConfig -> MetaConfig IncludeConfig -> b -> Compiler b
hakyllTut gcfg icfg doc = do
  meta <- getMetadata =<< getUnderlying
  runIOCompiler $
    runResourceT $ doTransformation' (tutTransformation gcfg icfg) meta doc

defaultHakyllTut :: Walkable Block b => b -> Compiler b
defaultHakyllTut = hakyllTut ghciMetaConfig includeMetaConfig
