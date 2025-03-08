{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Traversable.Unzip.EagerPair (EagerPair (..)) where
import Data.Biapplicative (Bifunctor (..), Biapplicative (..))

newtype EagerPair a b = EagerPair { unEagerPair :: (a, b) }
  deriving newtype (Functor)

instance Bifunctor EagerPair where
  bimap f g (EagerPair (a, b)) = EagerPair (f a, g b)

instance Biapplicative EagerPair where
  bipure a b = EagerPair (a, b)
  biliftA2 f g (EagerPair (a, b)) (EagerPair (x, y))
    = EagerPair (f a x, g b y)
