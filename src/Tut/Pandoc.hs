{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tut.Pandoc ( tut ) where

import           Data.List.Split
import qualified Data.Map               as M
import qualified Data.Text              as T
import           Language.Haskell.Ghcid hiding ( loadFile )
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.Walk
import           Tut.Config
import           Tut.Misc
import           Control.Arrow

newtype ParsedInclude = ParsedInclude { lookupBlock :: Label -> Maybe Text }

newtype Includes = Includes { lookupInclude :: Name -> Maybe (Language, Text) }

includes :: (MonadError e m, MonadReader IncludeConfig m, MonadLoadFile m
            , AsUnsupportedFileExt e)
         => m Includes
includes = do
    (p, l) <- reader (includeFiles &&& languages)
    let lng k = maybe (throwError (unsupportedFileExt k)) return (M.lookup k l)
        f fp = (,) <$> lng (takeExtension fp) <*> loadFile fp
    Includes . flip M.lookup <$> traverse f p

getBlock :: (MonadError e m, AsBlockNotFound e)
         => Label
         -> Name
         -> ParsedInclude
         -> m Text
getBlock lbl n i = maybeError (blockNotFound n lbl) (lookupBlock i lbl)

getInclude :: (MonadError e m, AsIncludeNotFound e)
           => Includes
           -> String
           -> m (Language, Text)
getInclude incs n = maybeError (includeNotFound n) (lookupInclude incs n)

parseBlocks :: MonadReader IncludeConfig f
            => LabelParser
            -> Text
            -> f [(String, Text)]
parseBlocks lp t = flip fmap (reader headerLabel) $
    \hl -> case runLabelParser lp of
        Nothing -> [ (hl, t) ]
        Just lp' -> let (foo, bar) = betterSplit g (T.lines t)
                        g ln = maybeError ln (lp' ln)
                    in
                        fmap T.unlines <$> (hl, foo) : bar

parseInclude :: MonadReader IncludeConfig m
             => (Language, Text)
             -> m ParsedInclude
parseInclude (l, t) = do
    fx <- reader fixBlock
    blks <- parseBlocks (labelParser l) t
    return $ ParsedInclude (flip M.lookup . fmap fx $ M.fromList blks)

keywordNameBlock :: Monad m
                 => String
                 -> (String
                     -> (String, [String], [(String, String)])
                     -> String
                     -> m Block)
                 -> Block
                 -> m Block
keywordNameBlock k f bl@(CodeBlock (i, s : classes, nvs) c) =
    case splitOn "/" s of
        [ x, n ] | x == k -> g n
        [ x ] | x == k -> g ""
        _ -> return bl
  where
    g n = f n (i, classes, nvs) c
keywordNameBlock _ _ bl =
    return bl

walkKeywordNameBlocks :: (Monad m, Walkable Block b)
                      => String
                      -> (String
                          -> (String, [String], [(String, String)])
                          -> String
                          -> m Block)
                      -> b
                      -> m b
walkKeywordNameBlocks k f =
    walkM (keywordNameBlock k f)

includeF :: (MonadError e m, MonadReader IncludeConfig m
            , AsIncludeNotFound e, AsBlockNotFound e)
         => Includes
         -> String
         -> (String, [String], [(String, String)])
         -> String
         -> m Block
includeF incs n (i, classes, nvs) c = do
    (l, t) <- getInclude incs n
    p <- parseInclude (l, t)
    c' <- traverse (\lbl -> getBlock lbl n p) (words c)
    return $
        CodeBlock (i, languageClasses l ++ classes, nvs)
                  (T.unpack . T.unlines $ c')

include :: (MonadError e m, MonadReader IncludeConfig m, Walkable Block b
           , AsUnsupportedFileExt e, AsIncludeNotFound e, AsBlockNotFound e
           , MonadLoadFile m)
        => b
        -> m b
include d = do
    k <- reader includeKeyword
    incs <- includes
    walkKeywordNameBlocks k (includeF incs) d

withGhciSessions :: (MonadIO m, MonadReader GhciConfig m)
                 => (Map String Ghci -> m b)
                 -> m b
withGhciSessions f = do
    s <- reader sessions
    ss <- traverse startGhciCmd s
    f ss <* traverse_ (liftIO . stopGhci) ss

startGhciCmd :: MonadIO m => GhciCmd -> m Ghci
startGhciCmd = liftIO . fmap fst . (startGhci <$> command <*> workingDir <*> callback)

getSession :: (MonadError e m, AsSessionNotFound e)
           => String
           -> Map String a
           -> m a
getSession = lookupError sessionNotFound

displayExec :: (MonadIO m, MonadReader GhciConfig m)
            => Ghci
            -> String
            -> m String
displayExec g e = do
    rs <- liftIO $ exec g e
    d <- reader display
    return $ d e rs

ghciF :: (MonadIO m, MonadReader GhciConfig m, MonadError e m, AsSessionNotFound e)
      => Map String Ghci
      -> String
      -> (String, [String], [(String, String)])
      -> String
      -> m Block
ghciF gs n (i, classes, nvs) c = do
    g <- getSession n gs
    newC <- traverse (displayExec g) (lines c)
    sk <- reader silenceKeyword
    return $
        if sk `elem` classes
        then Null
        else CodeBlock (i, "haskell" : classes, nvs) (unlines newC)

ghci :: (MonadIO m, MonadError e m, MonadReader GhciConfig m, Walkable Block b
        , AsSessionNotFound e)
     => b
     -> m b
ghci doc = do
    k <- reader ghciKeyword
    withGhciSessions $ \m -> walkKeywordNameBlocks k (ghciF m) doc

tutInclude
  :: (MonadError e m, MonadLoadFile m, AsUnsupportedFileExt e,
      AsIncludeNotFound e, AsBlockNotFound e, Walkable Block a) =>
     IncludeConfig -> a -> m a
tutInclude = flip $ runReaderT . include

tutGhci :: (MonadIO m, MonadError e m, AsSessionNotFound e, Walkable Block a)
        => GhciConfig
        -> a
        -> m a
tutGhci = flip $ runReaderT . ghci

tut :: (MonadIO m, MonadError e m, Walkable Block b, AsSessionNotFound e
       , AsUnsupportedFileExt e, AsIncludeNotFound e, AsBlockNotFound e
       , MonadLoadFile m)
    => (Maybe IncludeConfig, Maybe GhciConfig)
    -> b
    -> m b
tut (a, b) = maybe return tutInclude a <=< maybe return tutGhci b
