{-# LANGUAGE NoMonomorphismRestriction #-}

module Tut.Misc
    ( module Tut.Misc
    , module X
    , Map
    , Text
    ) where

import           Data.Map               ( Map )
import qualified Data.Map               as M
import           Data.Text              ( Text )
import           Control.Monad.Except   as X
import           Control.Monad.Reader   as X
import           Control.Monad.IO.Class as X
import           Control.Applicative    as X
import           Data.Foldable          as X
import qualified Data.Text              as T

import           Data.Char              ( isSpace )
import           Data.List              ( dropWhileEnd )

maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e = maybe (throwError e) return

eitherError :: MonadError e m => Either e a -> m a
eitherError = either throwError return

lookupError :: (Ord k, MonadError e m) => (k -> e) -> k -> Map k a -> m a
lookupError e x = maybeError (e x) . M.lookup x

-- | Split a list on elements which satisfy a predicate, using `Either b c` instead of `Bool` to provide type info about whether or not the resulting values satisfy the predicate
betterSplit :: (a -> Either b c) -> [a] -> ([b], [(c, [b])])
betterSplit _ [] = ([], [])
betterSplit f (x : xs) =
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

fixTextIndent :: Text -> Text
fixTextIndent t = T.unlines (T.drop minIndent <$> dropAround allWhiteSpace ts)
  where
    ts = T.lines t
    minIndent = (minimum . map indentation) ts

collapseErrors :: MonadError e1 m => (e -> e1) -> ExceptT e m a -> m a
collapseErrors f = either (throwError . f) return <=< runExceptT
