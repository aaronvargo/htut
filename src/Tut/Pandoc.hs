module Tut.Pandoc
  ( module Tut.Pandoc
  , module Text.Pandoc
  ) where

import Text.Pandoc
import Text.Pandoc.Error
import Tut.Misc
import Tut.Imports
import Tut.Transformation
import Tut.Metadata

class AsYamlParseError e =>
      AsPandocError e  where
  pandocError :: PandocError -> e

instance AsPandocError String where
  pandocError = show

readTransformMarkdown
  :: (AsPandocError e, MonadError e m)
  => ReaderOptions -> TransformationT m a -> String -> m (a, Pandoc)
readTransformMarkdown r t s = do
  (meta, contents) <- parsePage s
  doc <- eitherError . left pandocError $ readMarkdown r contents
  doTransformation t meta doc

transformMarkdown
  :: (MonadError e f, AsPandocError e)
  => String
  -> ReaderOptions
  -> WriterOptions
  -> TransformationT f a
  -> f (a, String)
transformMarkdown input rOpts wOpts t =
  fmap (writeMarkdown wOpts) <$> readTransformMarkdown rOpts t input

transformMarkdownFile
  :: (MonadError e m, MonadBase IO m, AsPandocError e)
  => FilePath
  -> FilePath
  -> ReaderOptions
  -> WriterOptions
  -> TransformationT m a
  -> m a
transformMarkdownFile input output rOpts wOpts t = do
  contents <- liftBase $ readFile input
  (a, s) <- transformMarkdown contents rOpts wOpts t
  liftBase $ writeFile output s
  return a
