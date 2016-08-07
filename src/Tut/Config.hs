{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module Tut.Config where

import qualified Data.Text              as T
import           Language.Haskell.Ghcid hiding ( loadFile )
import           Tut.Misc
import qualified Data.Map               as M
import qualified Data.Text.IO           as T

type Name = String

type Label = String

class AsBlockNotFound e where
    blockNotFound :: Name -> Label -> e

class AsIncludeNotFound e where
    includeNotFound :: String -> e

class AsUnsupportedFileExt e where
    unsupportedFileExt :: String -> e

class AsSessionNotFound e where
    sessionNotFound :: String -> e

data Error = BlockNotFound Name Label
           | IncludeNotFound String
           | UnsupportedFileExt String
           | SessionNotFound String
    deriving (Show, Eq)

instance AsBlockNotFound Error where
    blockNotFound = BlockNotFound

instance AsIncludeNotFound Error where
    includeNotFound = IncludeNotFound

instance AsUnsupportedFileExt Error where
    unsupportedFileExt = UnsupportedFileExt

instance AsSessionNotFound Error where
    sessionNotFound = SessionNotFound

class Monad m =>
      MonadLoadFile m where
    loadFile :: FilePath -> m Text

instance MonadLoadFile IO where
    loadFile = T.readFile

instance (Monad (t m), MonadTrans t, MonadLoadFile m) =>
         MonadLoadFile (t m) where
    loadFile = lift . loadFile

data IncludeConfig = IncludeConfig { includeKeyword :: String
                                   , headerLabel    :: String
                                   , fixBlock       :: Text -> Text
                                   , includeFiles   :: Map Name FilePath
                                   , languages      :: Map String Language
                                   }

data Language = Language { languageClasses :: [String]
                         , labelParser     :: LabelParser
                         }

-- | LabelParser Nothing = LabelParser (Just (const Nothing))
newtype LabelParser = LabelParser { runLabelParser :: Maybe (Text -> Maybe Label)
                                  }

--TODO Allow each ghci cmd/session to have its own display function. Would allow different prompts for different sessions

--TODO support including all sources from a project (given by a session)

data GhciConfig = GhciConfig { ghciKeyword    :: String
                             , silenceKeyword :: String
                             , display        :: String -> [String] -> String
                             , sessions       :: Map String GhciCmd
                             }

data GhciCmd = GhciCmd { command      :: String
                       , workingDir      :: Maybe String
                       , callback :: Stream -> String -> IO ()
                       }

prefixLabelParser :: String -> Text -> Maybe Label
prefixLabelParser p = fmap (T.unpack . T.strip) .
    T.stripPrefix (T.pack p) . T.strip

lineCommentLabelParser :: String -> LabelParser
lineCommentLabelParser =
    LabelParser . Just . prefixLabelParser . (++ "label")

nullLabelParser :: LabelParser
nullLabelParser = LabelParser Nothing

defaultIncludeConfig :: IncludeConfig
defaultIncludeConfig = IncludeConfig "include"
                                     "header"
                                     fixTextIndent
                                     M.empty
                                     defaultLanguages
  where
    defaultLanguages = M.fromList [ f ".hs" "haskell" "--"
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
                                  , md ".md"
                                  , md ".markdown"
                                  ]
    f a b c = (a, Language [ b ] . lineCommentLabelParser $ c)
    md a = f a "markdown" "<!---"

promptDisplay :: String -> String -> [String] -> String
promptDisplay p e rs = unlines $ (p ++ " " ++ e) : rs

defaultGhciConfig :: GhciConfig
defaultGhciConfig = GhciConfig "ghci"
                               "silent"
                               (promptDisplay "λ>")
                               M.empty

printGhciCallback Stderr s = putStrLn $ "stderr: " ++ s
printGhciCallback Stdout s = putStrLn $ "stdout: " ++ s

defaultGhciCmd :: FilePath -> GhciCmd
defaultGhciCmd dir = GhciCmd "stack ghci" (Just dir) (const $ const (return ()))
