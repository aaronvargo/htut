{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Tut.Transformation
  ( module Tut.Transformation
  , module Text.Pandoc.CodeBlock
  , MonadError
  , Walkable
  , Map
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

import Tut.Json
import Tut.Misc
import Control.Category1
import Text.Pandoc.CodeBlock

import Text.Pandoc
import Text.Pandoc.Walk
import Data.Aeson.Types
import Data.Maybe
import Data.List.Split

type Keyword = Text

type Name = Text

data TutWalk m = TutWalk
  { blockTransformers :: Map Keyword (Name -> CodeBlock -> m Block)
  , finalizer :: m ()
  }

instance Functor1 TutWalk where
  map1 f (TutWalk m r) = TutWalk ((fmap . fmap . fmap) f m) (f r)

instance Applicative m => Monoid (TutWalk m) where
  mappend (TutWalk a b) (TutWalk a' b') = TutWalk (mappend a a') (b *> b')
  mempty = TutWalk mempty (pure ())

walkKeywordNameBlocks
  :: (Monad m, Walkable Block b)
  => (Keyword -> Name -> CodeBlock -> m (Maybe Block)) -> b -> m b
walkKeywordNameBlocks f =
  walkM $
  \case
    bl@(CodeBlock (i, s:cs, nvs) c) ->
      let g k n =
            fromMaybe bl <$>
            f (T.pack k) (T.pack n) (CodeBlock_ (i, cs, nvs) (T.pack c))
      in case splitOn ":" s of
           [k, n] -> g k n
           [k] -> g k ""
           _ -> return bl
    bl -> return bl

walkTut :: (Monad m, Walkable Block b) => TutWalk m -> b -> m b
walkTut (TutWalk m r) doc = liftM2 const (walkKeywordNameBlocks f doc) r
  where f k n = sequence <$> traverse ($ n) (M.lookup k m)

nameWalk
  :: (MonadError e m, AsTransformationError e)
  => Keyword -> Map Name (CodeBlock -> m Block) -> Map Keyword (Name -> CodeBlock -> m Block)
nameWalk k m = M.singleton k f
  where f n cb = lookupError nameNotFound n m >>= ($ cb)

newtype TransformationT m a = TransformationT
  { runTransformation :: ReaderT Object (WriterT (TutWalk m) m) a
  } deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadTrans TransformationT where
  lift = TransformationT . lift . lift

instance MFunctor TransformationT where
  hoist f = mapTransformationT (f . (fmap . fmap) (map1 f))

deriving instance MonadBase b m => MonadBase b (TransformationT m)

deriving instance
         MonadState s m => MonadState s (TransformationT m)

deriving instance
         MonadError e m => MonadError e (TransformationT m)

instance MonadReader r m =>
         MonadReader r (TransformationT m) where
  ask = lift ask
  local f = mapTransformationTWriter (local f)

instance MonadWriter w m =>
         MonadWriter w (TransformationT m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapTransformationT (fmap g . listen)
    where
      g ((a, b), w) = ((a, w), b)
  pass = mapTransformationT (pass . fmap g)
    where
      g ((a, b), w) = ((a, w), b)

instance (Monoid a, Applicative m) =>
         Monoid (TransformationT m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

mapTransformationT
  :: (m (a, TutWalk m) -> n (b, TutWalk n))
  -> TransformationT m a
  -> TransformationT n b
mapTransformationT f = mapTransformationTWriter (mapWriterT f)

mapTransformationTWriter
  :: (WriterT (TutWalk m) m a -> WriterT (TutWalk n) n b)
  -> TransformationT m a
  -> TransformationT n b
mapTransformationTWriter f =
  TransformationT . ReaderT . fmap f . runReaderT . runTransformation

transformation
  :: Functor m
  => (Object -> m (TutWalk m, a))
  -> TransformationT m a
transformation = TransformationT . ReaderT . fmap (WriterT . fmap (snd &&& fst))

-- metaTransformation
--   :: ( MonadError e m
--      , AsUndefinedField e
--      , Traversable_1 f
--      , Applicative1 f
--      , AsTransformationError e
--      )
--   => ReaderT (f Identity) m (CodeBlock -> ReaderT (f Identity) m Block, ReaderT (f Identity) m (), a)
--   -> MetaConfig f
--   -> TransformationT m [(Name, a)]
-- metaTransformation r cfg =
--   transformation $
--   \o -> do
--     m <- (traverse . traverse) f =<< parseTutMeta cfg o
--     let fs = M.fromList $ (fmap . fmap) (view _1) m
--         fin = mapM_ (view (_2._2)) m
--         as = (fmap . fmap) (view _3) m
--     return (TutWalk (nameWalk (_topField cfg) fs) fin, as)
--   where
--     f c = runReaderT ((fmap . over _1 . fmap) (flip runReaderT c) r) c

metaTransformation
  :: ( MonadError e m
     , AsUndefinedField e
     , Traversable_1 f
     , Applicative1 f
     , AsTransformationError e
     )
  => (f Identity -> m (CodeBlock -> m Block, m (), a))
  -> MetaConfig f
  -> TransformationT m [(Name, a)]
metaTransformation f cfg =
  transformation $
  \o -> do
    m <- (traverse . traverse) f =<< parseTutMeta cfg o
    let fs = M.fromList $ (fmap . fmap) (view _1) m
        fin = mapM_ (view (_2._2)) m
        as = (fmap . fmap) (view _3) m
    return (TutWalk (nameWalk (_topField cfg) fs) fin, as)

class AsParseError e =>
      AsTransformationError e  where
  nameNotFound :: Name -> e

instance AsTransformationError String where
  nameNotFound = ("Name not found: " ++) . T.unpack

doTransformation
  :: (Monad m, Walkable Block b)
  => TransformationT m a -> Object -> b -> m (a, b)
doTransformation trns o doc = do
  (a, tw) <- runWriterT (runReaderT (runTransformation trns) o)
  (a, ) <$> walkTut tw doc

doTransformation'
  :: (Monad f, Walkable Block b)
  => TransformationT f a -> Object -> b -> f b
doTransformation' = (fmap . fmap . fmap . fmap) snd doTransformation
