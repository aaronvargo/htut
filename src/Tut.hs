module Tut
  ( module Tut
  , module X
  ) where

import Tut.Transformation as X
import Tut.Include as X
import Tut.Ghci as X
import Tut.Json
import Tut.Repl

tutTransformation
  :: ( MonadError e m
     , MonadResource m
     , AsUndefinedField e
     , AsTransformationError e
     , AsReplError e
     , IncludeError e
     , MonadLoadFile m
     )
  => MetaConfig GhciConfig -> MetaConfig IncludeConfig -> TransformationT m ()
tutTransformation gcfg icfg = do
  ghciTransformation gcfg >>= includeTransformationWithConfigs icfg

defaultTutTransformation
  :: ( MonadError e m
     , MonadResource m
     , AsUndefinedField e
     , AsTransformationError e
     , AsReplError e
     , IncludeError e
     , MonadLoadFile m
     )
  => TransformationT m ()
defaultTutTransformation = tutTransformation ghciMetaConfig includeMetaConfig
