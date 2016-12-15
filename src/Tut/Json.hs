{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Tut.Json
  ( module Tut.Json
  ) where

import Tut.Misc
import Control.Category1
import Data.Aeson.Types

import qualified Data.Text as T

maybeFieldW :: Object -> Text -> (Value -> Parser a) -> Parser (Maybe a)
maybeFieldW v k p = v .:? k >>= traverse p

textCaseParser :: [(Text, b)] -> Value -> Parser b
textCaseParser xs v = do
  t <- (parseJSON v :: Parser Text)
  maybe empty return $ lookup t xs

data Field a = Field
  { fieldName :: Text
  , fieldParser :: Value -> Parser a
  , fieldSemigroup :: a -> a -> a
  , fieldDefault :: Maybe a
  }

field
  :: FromJSON a
  => Text -> Field a
field n = Field n parseJSON const Nothing

fieldWDef
  :: FromJSON a
  => Text -> a -> Field a
fieldWDef n d =
  (field n)
  { fieldDefault = Just d
  }

data MetaConfig f = MetaConfig
  { _topField :: Text
  , _configFields :: f Field
  , _configFromString :: Text -> (Text, f Maybe)
  , _extraConfigs :: Endo [(Text, f Maybe)]
  , _nameField :: Text
  , _defaultsField :: Text
  , _configsField :: Text
  }

defaultMetaConfig :: Text -> f Field -> (Text -> f Maybe) -> MetaConfig f
defaultMetaConfig tf flds f =
  MetaConfig
  { _topField = tf
  , _configFields = flds
  , _configFromString = (,) "" . f
  , _extraConfigs = mempty
  , _nameField = "name"
  , _defaultsField = "defaults"
  , _configsField = "configs"
  }

topField
  :: Functor f
  => (Text -> f Text) -> MetaConfig f1 -> f (MetaConfig f1)
topField ff (MetaConfig a b c d e f g) =
  fmap (\a' -> MetaConfig a' b c d e f g) (ff a)

configFields
  :: Functor f
  => (f1 Field -> f (f1 Field)) -> MetaConfig f1 -> f (MetaConfig f1)
configFields ff (MetaConfig a b c d e f g) =
  fmap (\b' -> MetaConfig a b' c d e f g) (ff b)

configFromString
  :: Functor f
  => ((Text -> (Text, f1 Maybe)) -> f (Text -> (Text, f1 Maybe)))
  -> MetaConfig f1
  -> f (MetaConfig f1)
configFromString ff (MetaConfig a b c d e f g) =
  fmap (\c' -> MetaConfig a b c' d e f g) (ff c)

extraConfigs
  :: Functor f
  => (Endo [(Text, f1 Maybe)] -> f (Endo [(Text, f1 Maybe)]))
  -> MetaConfig f1
  -> f (MetaConfig f1)
extraConfigs ff (MetaConfig a b c d e f g) =
  fmap (\d' -> MetaConfig a b c d' e f g) (ff d)

nameField
  :: Functor f
  => (Text -> f Text) -> MetaConfig f1 -> f (MetaConfig f1)
nameField ff (MetaConfig a b c d e f g) =
  fmap (\e' -> MetaConfig a b c d e' f g) (ff e)

defaultsField
  :: Functor f
  => (Text -> f Text) -> MetaConfig f1 -> f (MetaConfig f1)
defaultsField ff (MetaConfig a b c d e f g) =
  fmap (\f' -> MetaConfig a b c d e f' g) (ff f)

configsField
  :: Functor f
  => (Text -> f Text) -> MetaConfig f1 -> f (MetaConfig f1)
configsField ff (MetaConfig a b c d e f g) =
  fmap (\g' -> MetaConfig a b c d e f g') (ff g)

metaConfig
  :: Functor1 f
  => Text -> f Field -> Setter' (f Maybe) (Maybe Text) -> MetaConfig f
metaConfig tf flds l =
  defaultMetaConfig tf flds (\t -> set l (Just t) (map1 fieldDefault flds))

class AsParseError e  where
  parseError :: String -> e

instance AsParseError String where
  parseError = ("Error parsing json: " ++)

runParser
  :: (MonadError e m, AsParseError e)
  => Parser a -> m a
runParser p = eitherError . left parseError $ parseEither (const p) ()

class AsUndefinedField e  where
  undefinedField :: Text -> e

instance AsUndefinedField String where
  undefinedField = ("Undefined field: " ++) . T.unpack

parseTutMeta
  :: forall e m f.
     ( MonadError e m
     , AsParseError e
     , AsUndefinedField e
     , Traversable_1 f
     , Applicative1 f
     )
  => MetaConfig f -> Object -> m [(Text, f Identity)]
parseTutMeta cfg o =
  runParser configs >>=
  (traverse . traverse)
    (completeFields . wDefConfigs (map1 fieldDefault fields))
  where
    fields = (_configFields cfg)
    --
    configs :: Parser [(Text, f Maybe)]
    configs = maybeFieldW o (_topField cfg) parseConfigs .!= withExtraConfigs []
    --
    withExtraConfigs = appEndo (_extraConfigs cfg)
    --
    parseConfigs :: Value -> Parser [(Text, f Maybe)]
    parseConfigs (String t) =
      return $ withExtraConfigs [_configFromString cfg t]
    parseConfigs (Object v) = do
      d <- maybeFieldW v (_defaultsField cfg) parseConfig .!= pure1 Nothing
      cs <- v .:? _configsField cfg .!= [] >>= traverse parseNamed
      return $ (fmap . fmap) (wDefConfigs d) (withExtraConfigs cs)
    parseConfigs j@(Array _) =
      fmap withExtraConfigs $ traverse parseNamed =<< parseJSON j
    parseConfigs _ = empty
    --
    parseNamed :: Value -> Parser (Text, f Maybe)
    parseNamed (Object v) = (,) <$> v .: _nameField cfg <*> parseConfigO v
    parseNamed _ = empty
    --
    parseConfig :: Value -> Parser (f Maybe)
    parseConfig (Object v) = parseConfigO v
    parseConfig _ = empty
    --
    parseConfigO :: Object -> Parser (f Maybe)
    parseConfigO v = traverse_1 (\(Field n p _ _) -> maybeFieldW v n p) fields
    --
    completeFields =
      sequence_1I . lift2_1 (maybeError . undefinedField . fieldName) fields
    --
    wDefConfigs = flip $ lift3_1 (maybeSemigroup . fieldSemigroup) fields
