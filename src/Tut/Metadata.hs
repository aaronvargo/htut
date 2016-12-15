{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tut.Metadata where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Yaml as Yaml
import Hakyll.Core.Metadata (Metadata)
import           Data.Maybe
import           Tut.Misc

class AsYamlParseError e where
  yamlParseException :: ParseException -> e

instance AsYamlParseError String where
  yamlParseException = prettyPrintParseException

parsePage
  :: (AsYamlParseError e, MonadError e m)
  => String -> m (Object, String)
parsePage = eitherError . left yamlParseException . parsePage'

-- Rest of file taken from Hakyll.Core.Provier.Metadata, which hakyll doesn't expose
------------------------------------------------------------------------------

-- | Like 'break', but can act on the entire tail of the list.
breakWhen :: ([a] -> Bool) -> [a] -> ([a], [a])
breakWhen predicate = go []
  where
    go buf []                = (reverse buf, [])
    go buf (x : xs)
        | predicate (x : xs) = (reverse buf, x : xs)
        | otherwise          = go (x : buf) xs

-- | Parse the page metadata and body.
splitMetadata :: String -> (Maybe String, String)
splitMetadata str0 = fromMaybe (Nothing, str0) $ do
    guard $ leading >= 3
    let !str1 = drop leading str0
    guard $ all isNewline (take 1 str1)
    let !(!meta, !content0) = breakWhen isTrailing str1
    guard $ not $ null content0
    let !content1 = drop (leading + 1) content0
        !content2 = dropWhile isNewline $ dropWhile isInlineSpace content1
    -- Adding this newline fixes the line numbers reported by the YAML parser.
    -- It's a bit ugly but it works.
    return (Just ('\n' : meta), content2)
  where
    -- Parse the leading "---"
    !leading = length $ takeWhile (== '-') str0

    -- Predicate to recognize the trailing "---" or "..."
    isTrailing []       = False
    isTrailing (x : xs) =
        isNewline x && length (takeWhile isDash xs) == leading

    -- Characters
    isNewline     c = c == '\n' || c == '\r'
    isDash        c = c == '-'  || c == '.'
    isInlineSpace c = c == '\t' || c == ' '

parseMetadata :: String -> Either Yaml.ParseException Metadata
parseMetadata = Yaml.decodeEither' . T.encodeUtf8 . T.pack

parsePage' :: String -> Either Yaml.ParseException (Metadata, String)
parsePage' fileContent = case mbMetaBlock of
    Nothing        -> return (mempty, content)
    Just metaBlock -> case parseMetadata metaBlock of
        Left  err  -> Left   err
        Right meta -> return (meta, content)
  where
    !(!mbMetaBlock, !content) = splitMetadata fileContent
