{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tut.Misc
  ( module Tut.Misc
  , module Tut.Imports
  ) where

import Tut.Imports

import qualified Data.Text as T
import Data.Char (isSpace)
import Data.Key
import Data.List
import System.Directory
import System.FilePath

headMay
  :: Foldable t
  => t a -> Maybe a
headMay = foldr (const . Just) Nothing

maybeSemigroup :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeSemigroup f (Just x) (Just y) = Just (f x y)
maybeSemigroup _ a b = a <|> b

maybeError
  :: MonadError e m
  => e -> Maybe a -> m a
maybeError e = maybe (throwError e) return

eitherError
  :: MonadError e m
  => Either e a -> m a
eitherError = either throwError return

lookupError
  :: (MonadError e m, Lookup f)
  => (Key f -> e) -> Key f -> f a -> m a
lookupError e x = maybeError (e x) . Data.Key.lookup x

handleErrors
  :: Monad m
  => (e -> m a) -> ExceptT e m a -> m a
handleErrors f = either f return <=< runExceptT

collapseErrors
  :: MonadError e1 m
  => (e -> e1) -> ExceptT e m a -> m a
collapseErrors f = handleErrors (throwError . f)

-- | Split a list on elements which satisfy a predicate, using `Either b c` instead of `Bool` to provide type info about whether or not the resulting values satisfy the predicate
betterSplit :: (a -> Either b c) -> [a] -> ([b], [(c, [b])])
betterSplit _ [] = ([], [])
betterSplit f (x:xs) =
  case f x of
    Left y -> (y : ys, zs)
    Right y -> ([], (y, ys) : zs)
  where
    (ys, zs) = betterSplit f xs

dropAround :: (a -> Bool) -> [a] -> [a]
dropAround f = dropWhileEnd f . dropWhile f

indentation :: Text -> Int
indentation = T.length . T.takeWhile isSpace

allWhiteSpace :: Text -> Bool
allWhiteSpace = T.null . T.strip

stripWhiteSpaceLines :: [Text] -> [Text]
stripWhiteSpaceLines = dropAround allWhiteSpace

fixTextIndent :: Text -> Text
fixTextIndent t = T.unlines (T.drop minIndent <$> stripWhiteSpaceLines ts)
  where
    ts = T.lines t
    minIndent = (minimum . map indentation) ts

unlines1 :: [Text] -> Text
unlines1 = T.intercalate "\n"

relativizePath :: MonadBase IO m => FilePath -> m FilePath
relativizePath pth = (flip makeRelative pth) <$> liftBase getCurrentDirectory
