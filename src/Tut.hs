{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tut
  ( module Tut
  , module X
  ) where

import Tut.Transformation as X
import Tut.Include as X
import Tut.Ghci as X
import Tut.Json
import Tut.Repl
import Tut.Misc

tutTransformation
  :: ( MonadLoadFile m
     , IncludeError e
     , AsReplError e
     , AsTransformationError e
     , AsUndefinedField e
     , MonadError e m
     , MonadBase IO m
     )
  => Allocator m
  -> MetaConfig GhciConfig
  -> MetaConfig IncludeConfig
  -> TransformationT m ()
tutTransformation (allc :: Allocator _) gcfg icfg = do
  ghciTransformation allc gcfg >>= includeTransformationWithConfigs icfg

-- defaultTutTransformation
--   :: ( MonadError e m
--      , MonadResource m
--      , AsUndefinedField e
--      , AsTransformationError e
--      , AsReplError e
--      , IncludeError e
--      , MonadLoadFile m
--      )
--   => TransformationT m ()
-- defaultTutTransformation = tutTransformation ghciMetaConfig includeMetaConfig
