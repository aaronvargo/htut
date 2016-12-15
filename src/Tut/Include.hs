{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Tut.Include where

import Tut.Transformation
import Tut.Misc
import Tut.Json
import Control.Category1

import Data.Text.Strict.Lens
import System.FilePath
import Text.Pandoc

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Label = Text

-- | LabelParser Nothing = LabelParser (Just (const Nothing))
newtype LabelParser = LabelParser
  { runLabelParser :: Maybe (Text -> Maybe Label)
  }

data Language = Language
  { languageClasses :: [String]
  , labelParser :: LabelParser
  }

data IncludeConfig f = IncludeConfig
  { _formatter :: f ([Text] -> Text)
  , _languages :: f (Map String Language)
  , _file :: f FilePath
  }

formatter :: Lens' (IncludeConfig f) (f ([Text] -> Text))
formatter f (IncludeConfig a b c) = fmap (\a' -> IncludeConfig a' b c) (f a)

languages :: Lens' (IncludeConfig f) (f (Map String Language))
languages f (IncludeConfig a b c) = fmap (\b' -> IncludeConfig a b' c) (f b)

file :: Lens' (IncludeConfig f) (f FilePath)
file f (IncludeConfig a b c) = fmap (\c' -> IncludeConfig a b c') (f c)

formatter'
  :: MonadReader (IncludeConfig Identity) m
  => m ([Text] -> Text)
formatter' = reader $ runIdentity . _formatter

languages'
  :: MonadReader (IncludeConfig Identity) m
  => m (Map String Language)
languages' = reader $ runIdentity . _languages

file'
  :: MonadReader (IncludeConfig Identity) m
  => m FilePath
file' = reader $ runIdentity . _file

instance Functor1 IncludeConfig where
  map1 = record1Map1

instance Apply1 IncludeConfig where
  ap1 = record1Ap1

instance Applicative1 IncludeConfig where
  pure1 = record1Pure1

instance Traversable_1 IncludeConfig where
  sequence_1 = record1Sequence_1

instance Record1 IncludeConfig where
  record1Builder =
    Constructor IncludeConfig $ _formatter <::> _languages <::> _file <::> Nil

plainFormatter :: [Text] -> Text
plainFormatter = unlines1

simpleFormatter :: [Text] -> Text
simpleFormatter = unlines1 . stripWhiteSpaceLines

fixIndentFormatter :: [Text] -> Text
fixIndentFormatter = unlines1 . fmap fixTextIndent

lineCommentLabelParser :: Text -> LabelParser
lineCommentLabelParser =
  LabelParser . Just . prefixLabelParser . (`mappend` "label")
  where prefixLabelParser p = fmap (T.strip) . T.stripPrefix p . T.strip

nullLabelParser :: LabelParser
nullLabelParser = LabelParser Nothing

defaultLanguages :: Map String Language
defaultLanguages =
  M.fromList
    [ f ".hs" "haskell" "--"
    , f ".idr" "idris" "--"
    , f ".agda" "agda" "--"
    , f ".scala" "scala" "//"
    , f ".java" "java" "//"
    , f ".sh" "bash" "#"
    , f ".py" "python" "#"
    , f ".js" "javascript" "//"
    , f ".c" "c" "//"
    , f ".cc" "cpp" "//"
    , f ".cpp" "cpp" "//"
    , f ".cs" "cs" "//"
    , f ".md" "markdown" "<!---"
    , f ".markdown" "markdown" "<!---"
    ]
  where
    f a b c = (a, Language [b] $ lineCommentLabelParser c)

includeMetaConfig :: MetaConfig IncludeConfig
includeMetaConfig = metaConfig "include" flds (file . mapping packed)
  where
    flds =
      IncludeConfig
      { _formatter = Field "formatter" parseFrmt const (Just fixIndentFormatter)
      , _languages =
        Field "languages" (const empty) M.union (Just defaultLanguages)
      , _file = Tut.Json.field "file"
      }
    parseFrmt = textCaseParser [("none", plainFormatter)
                               , ("simple", simpleFormatter)
                               , ("fixIndent", fixIndentFormatter)
                               ]

includeTransformation
  :: ( MonadError e m
     , AsUndefinedField e
     , AsTransformationError e
     , IncludeError e
     , MonadLoadFile m
     )
  => MetaConfig IncludeConfig -> TransformationT m ()
includeTransformation = void <$> metaTransformation ((, ()) <$> includeBlock)

includeTransformationWithConfigs
  :: ( MonadError e m
     , AsUndefinedField e
     , AsTransformationError e
     , IncludeError e
     , MonadLoadFile m
     )
  => MetaConfig IncludeConfig
  -> [(Text, IncludeConfig Maybe)]
  -> TransformationT m ()
includeTransformationWithConfigs c cs =
  includeTransformation $ (extraConfigs <>~ Endo (++ cs)) c

class Monad m =>
      MonadLoadFile m  where
  loadFile :: FilePath -> m Text

instance MonadLoadFile IO where
  loadFile = T.readFile

instance (Monad (t m), MonadTrans t, MonadLoadFile m) =>
         MonadLoadFile (t m) where
  loadFile = lift . loadFile

class IncludeError e  where
  blockNotFound :: FilePath -> Label -> e
  unsupportedFileExt :: FilePath -> e

instance IncludeError String where
  blockNotFound fp l =
    "Label \"" ++ T.unpack l ++ "\" not found in file: " ++ fp
  unsupportedFileExt = ("Unsupported file extension: " ++)

parseBlocks
  :: Monad m
  => LabelParser -> Text -> m (Map Label Text)
parseBlocks lp t = do
  return . M.fromList $
    case runLabelParser lp of
      Nothing -> [("header", t)]
      Just lp' ->
        let (foo, bar) = betterSplit g (T.lines t)
            g ln = maybeError ln (lp' ln)
        in fmap T.unlines <$> ("header", foo) : bar

includeBlock
  :: ( MonadError e m
     , MonadError e1 m1
     , MonadReader (IncludeConfig Identity) m
     , IncludeError e
     , IncludeError e1
     , MonadLoadFile m
     )
  => m (CodeBlock -> m1 Block)
includeBlock = do
  (ls, fp) <- reader (languages' &&& file')
  Language lcs lp <- lookupError unsupportedFileExt (takeExtension fp) ls
  txt <- loadFile fp
  blcks <- parseBlocks lp txt
  frmt <- reader formatter'
  let getBlock lbl = maybeError (blockNotFound fp lbl) (M.lookup lbl blcks)
      trns =
        modClassesBody $
        \cs t -> do
          ts <- traverse getBlock (T.words t)
          return (lcs ++ cs, frmt ts)
  return trns
