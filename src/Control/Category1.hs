{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Category1
  ( Functor1
  , map1
  , Expo(Expo)
  , runExpo
  , Apply1
  , ap1
  , lift2_1
  , lift3_1
  , Applicative1
  , pure1
  , Traversable_1
  , traverse_1
  , sequence_1
  , sequence_1I
  , Record1Fields(Nil, Cons)
  , foldRecord1Fields
  , Record1Builder(Constructor)
  , (<::>)
  , foldRecord1Builder
  , Record1
  , record1Builder
  , record1
  , record1I
  , record1Map1
  , record1Ap1
  , record1Pure1
  , record1Sequence_1
  ) where

import Data.Functor.Identity
import Data.Functor.Compose
import Control.Applicative (liftA2)

class Functor1 t where
  map1 :: (forall α. f α -> g α) -> t f -> t g

newtype Expo f g a = Expo
  { runExpo :: f a -> g a
  }

class Functor1 t =>
      Apply1 t where
  ap1 :: t (Expo f g) -> t f -> t g

class Apply1 t =>
      Applicative1 t where
  pure1 :: (forall α. f α) -> t f

lift2_1
  :: Apply1 t
  => (forall α. f α -> g α -> h α) -> t f -> t g -> t h
lift2_1 f = ap1 . map1 (Expo . f)

lift3_1
  :: Apply1 t
  => (forall α. f α -> g α -> h α -> i α) -> t f -> t g -> t h -> t i
lift3_1 f x y z = (Expo . ((.) . (.)) Expo f) `map1` x `ap1` y `ap1` z

--TODO dont use default members
class Functor1 t =>
      Traversable_1 t where
  traverse_1
    :: Applicative g
    => (forall α. f α -> g (h α)) -> t f -> g (t h)
  traverse_1 f = sequence_1 . map1 (Compose . f)
  sequence_1
    :: Applicative f
    => t (Compose f g) -> f (t g)
  sequence_1 = traverse_1 getCompose

sequence_1I
  :: (Traversable_1 t, Applicative f)
  => t f -> f (t Identity)
sequence_1I = sequence_1 . map1 (Compose . fmap Identity)

data Record1Fields :: ((k -> *) -> *) -> (k -> *) -> * -> * where
        Nil :: Record1Fields t f (t f)
        Cons ::
          (forall φ . t φ -> φ a) ->
            Record1Fields t f b -> Record1Fields t f (f a -> b)

infixr 5 <::>
(<::>)
  :: (forall (φ :: * -> *). t φ -> φ a)
  -> Record1Fields t f b
  -> Record1Fields t f (f a -> b)
(<::>) = Cons

data Record1Builder :: ((k -> *) -> *) -> (k -> *) -> * where
        Constructor :: a -> Record1Fields t f a -> Record1Builder t f

foldRecord1Fields
  :: (forall α β. (forall φ. t φ -> φ α) -> r β -> r (f α -> β))
  -> r (t f)
  -> Record1Fields t f a
  -> r a
foldRecord1Fields _ z Nil = z
foldRecord1Fields f z (Cons x xs) = f x (foldRecord1Fields f z xs)

foldRecord1Builder
  :: (forall α. α -> r α -> b)
  -> (forall α β. (forall φ. t φ -> φ α) -> r β -> r (f α -> β))
  -> r (t f)
  -> Record1Builder t f
  -> b
foldRecord1Builder g f z (Constructor c flds) = g c (foldRecord1Fields f z flds)

record1
  :: forall t f r b.
     Record1 t
  => (forall α. α -> r α -> b)
  -> (forall α β. (forall φ. t φ -> φ α) -> r β -> r (f α -> β))
  -> r (t f)
  -> b
record1 g f z = foldRecord1Builder g f z record1Builder

class Record1 t where
  record1Builder :: Record1Builder t f

newtype F0 t f α = F0
  { runF0 :: α -> t f
  }

record1I
  :: forall t f.
     Record1 t
  => (forall α. (forall φ. t φ -> φ α) -> f α) -> t f
record1I f = record1 (flip runF0) (\φ (F0 g) -> F0 $ \h -> g . h $ f φ) (F0 id)

record1Ap1
  :: Record1 t
  => t (Expo f1 f) -> t f1 -> t f
record1Ap1 ff tf = record1I (\φ -> runExpo (φ ff) (φ tf))

record1Map1
  :: Record1 t
  => (forall α. f α -> g α) -> t f -> t g
record1Map1 f tf = record1I (\φ -> f (φ tf))

record1Pure1
  :: Record1 t
  => (forall α. f α) -> t f
record1Pure1 z = record1I (const z)

newtype F1 f t g α = F1 (f (α -> t g))

record1Sequence_1
  :: forall t f g.
     (Record1 t, Applicative f)
  => t (Compose f g) -> f (t g)
record1Sequence_1 tfg =
  record1
    (\a (F1 ff) -> ($ a) <$> ff)
    (\φ (F1 b) -> F1 . liftA2 (\x y f -> x (f y)) b . getCompose $ φ tfg)
    (F1 $ pure id)
