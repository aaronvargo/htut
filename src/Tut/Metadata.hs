{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
module Tut.Metadata where

import Tut.Misc
import Data.Yaml
import qualified Hakyll.Internal as H

class AsYamlParseError e  where
  yamlParseException :: ParseException -> e

instance AsYamlParseError String where
  yamlParseException = prettyPrintParseException

parsePage
  :: (AsYamlParseError e, MonadError e m)
  => String -> m (Object, String)
parsePage = eitherError . left yamlParseException . H.parsePage
