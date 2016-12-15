{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

module Tut.Ghci
  ( module Tut.Ghci
  , module Tut.Transformation
  , MonadResource
  , runResourceT
  , ReleaseKey
  , Load
  ) where

import Language.Haskell.Ghcid
import Tut.Include
import Tut.Repl hiding (prompt)
import Tut.Misc
import Control.Monad.Trans.Resource
import Data.Aeson.Types
import Text.Pandoc as P
import qualified Data.Text as T
import Tut.Json
import Tut.Transformation
import Data.Bool (bool)
import Control.Category1
import Data.Text.Strict.Lens
import Text.Regex
import Data.Maybe

data GhciConfig f = GhciConfig
  { _project :: f FilePath
  , _command :: f String
  , _callback :: f (Stream -> String -> IO ())
  , _prompt :: f Text
  }

project :: Lens' (GhciConfig f) (f FilePath)
project f (GhciConfig a b c d) = fmap (\a' -> GhciConfig a' b c d) (f a)

command :: Lens' (GhciConfig f) (f String)
command f (GhciConfig a b c d) = fmap (\b' -> GhciConfig a b' c d) (f b)

callback :: Lens' (GhciConfig f) (f (Stream -> String -> IO ()))
callback f (GhciConfig a b c d) = fmap (\c' -> GhciConfig a b c' d) (f c)

prompt :: Lens' (GhciConfig f) (f Text)
prompt f (GhciConfig a b c d) = fmap (\d' -> GhciConfig a b c d') (f d)

project'
  :: MonadReader (GhciConfig Identity) m
  => m FilePath
project' = reader $ runIdentity . _project

command'
  :: MonadReader (GhciConfig Identity) m
  => m String
command' = reader $ runIdentity . _command

callback'
  :: MonadReader (GhciConfig Identity) m
  => m (Stream -> String -> IO ())
callback' = reader $ runIdentity . _callback

prompt'
  :: MonadReader (GhciConfig Identity) m
  => m Text
prompt' = reader (runIdentity . _prompt)

instance Functor1 GhciConfig where
  map1 = record1Map1

instance Apply1 GhciConfig where
  ap1 = record1Ap1

instance Applicative1 GhciConfig where
  pure1 = record1Pure1

instance Traversable_1 GhciConfig where
  sequence_1 = record1Sequence_1

instance Record1 GhciConfig where
  record1Builder =
    Constructor GhciConfig $
    _project <::> _command <::> _callback <::> _prompt <::> Nil

printGhciCallback :: Stream -> String -> IO ()
printGhciCallback Stderr s = putStrLn $ "stderr: " ++ s
printGhciCallback Stdout s = putStrLn $ "stdout: " ++ s

nullCallback
  :: Monad m
  => b -> b1 -> m ()
nullCallback = const . const $ return ()

ghciMetaConfig :: MetaConfig GhciConfig
ghciMetaConfig = metaConfig "ghci" flds (project . mapping packed)
  where
    flds =
      GhciConfig
      { _project = field "project"
      , _command = fieldWDef "command" "stack ghci"
      , _callback =
        Field
          "callback"
          (fmap (bool printGhciCallback nullCallback) . parseJSON)
          const
          (Just nullCallback)
      , _prompt = fieldWDef "prompt" "λ"
      }

type Allocator m = forall a. IO a -> (a -> IO ()) -> m (m (), a)

allocator :: MonadResource m => Allocator m
allocator a f = (mapped . _1 %~ release) $ allocate a f

leakyAllocator :: MonadBase IO m => Allocator m
leakyAllocator a f = liftBase $ do
  x <- a
  return $ (liftBase $ f x, x)

ghciTransformation
  :: ( AsReplError e
     , AsTransformationError e
     , AsUndefinedField e
     , MonadBase IO m
     , MonadError e m
     )
  => Allocator m
  -> MetaConfig GhciConfig
  -> TransformationT m [(Text, IncludeConfig Maybe)]
ghciTransformation (allc :: Allocator _) =
  ((each . _2 . file . each %%~ relativizePath) . loadConfigs) <=<
  metaTransformation (ghciBlock allc)

loadConfig :: Load -> Maybe (Text, IncludeConfig Maybe)
loadConfig (Loading m f) =
  Just (T.toLower $ T.pack m, set file (Just f) (pure1 Nothing))
loadConfig (Message _ _ _ _) = Nothing

loadConfigs :: [(a, [Load])] -> [(Text, IncludeConfig Maybe)]
loadConfigs m = catMaybes . fmap loadConfig $ maybe [] snd (m ^? _head)

ghciBlock
  :: ( AsReplError e
     , MonadBase IO m1
     , Monad m
     , MonadError e m1
     )
  => Allocator m -> GhciConfig Identity -> m (CodeBlock -> m1 Block, m (), [Load])
ghciBlock (allc :: Allocator _) cfg = do
  (rls, (g, ls)) <-
    allc
      (startGhci (command' cfg) (Just (project' cfg)) (callback' cfg))
      (stopGhci . fst)
  let evl t = do
        rs <- liftBase $ exec g (T.unpack $ ":{\n" <> t <> "\n:}")
        let fl = any (isJust . matchRegex ghciErrorRegex) rs
            sr x = maybe x (!! 1) $ matchRegex (mkRegex "([^|]*\\| )\\1*(.*)") x
            res = unlines1 $ fmap (T.pack . sr) rs
        return $ ReplOutput res fl
      rpl = Repl ["haskell"] (prompt' cfg) evl "--"
  return (replBlock rpl, rls, ls)
  where
    ghciErrorRegex = mkRegex "^(<interactive>.*:)|(<[^>]*>:.*error:)"
