{-# LANGUAGE InstanceSigs #-}
module Compose where

import qualified Control.Arrow as CA (first)

newtype Compose f g a =
  Compose { getCompose :: f (g a) } deriving (Eq, Show) 
instance (Functor f, Functor g) =>
  Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure $ pure a
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) =
      Compose $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
  foldMap k (Compose fga) =
    (foldMap.foldMap) k fga

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
    traverse :: (Applicative h) =>
      (a -> h b) -> Compose f g a -> h (Compose f g b)
    traverse k (Compose fga) = Compose <$> (traverse . traverse) k fga

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b deriving (Show)
instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a deriving (Show)
instance Bifunctor Const where
  bimap f g (Const a) = Const $ f a

data Drei a b c = Drei a b c deriving (Show)
instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b deriving (Show)
instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a deriving (Show)
instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d deriving (Show)
instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Eitheri a b = Lefti a | Righti b deriving (Show)
instance Bifunctor Eitheri where
  bimap f g (Lefti a) = Lefti (f a)
  bimap f g (Righti b) = Righti (g b)


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap.fmap) f ma
instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ pure $ pure x
  EitherT f <*> EitherT a = EitherT $ (<*>) <$> f <*> a
instance (Monad m) => Monad (EitherT e m) where
  return = pure
  EitherT ma >>= f = EitherT $ do
    a <- ma
    case a of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ f <$> ma where
  f (Left e) = Right e
  f (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ma) = do
  v <- ma
  case v of
    Left a -> ma >>= f.(\(Left a) -> a)
    Right b -> ma >>= g.(\(Right b) -> b)
--OR
--ma >>= k f g where
  --k f g (Left a) = f a
  --k f g (Right b) = g b

--PROBABLY WRONG BECAUSE WE ARE THROWING AWAY THE STRUCTURE m
--  v <- ma
--  case v of
--    Left a -> f a
--    Right b -> g b


newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
instance (Functor m) => Functor (StateT s m) where
  fmap :: (a->b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) =
    -- ADVISED BY haskell-vim, WOW
    StateT $ (fmap.fmap) (CA.first f) smas
--    FOR FUNCTIONS, f <$> g == f.g
--    StateT $ (fmap.fmap) (\(a,s) -> (f a, s)) smas
--  fmap f (StateT smas) = StateT $ \s ->
--    let mas = smas s in (\(a,s) -> (f a, s)) <$> mas
instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> return (a,s)
  (<*>) :: StateT s m (a->b) -> StateT s m a -> StateT s m b
  StateT smf <*> StateT smas = StateT $ \s0 -> do
    (f,s1) <- smf s0
    (a,s2) <- smas s1
    return (f a, s2)
instance (Monad m) => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT smas >>= f = StateT $ \s0 -> do
    (a,s1) <- smas s0
    (runStateT $ f a) s1
