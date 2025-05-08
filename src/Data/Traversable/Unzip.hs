{-# LANGUAGE Trustworthy #-}  -- For safe coercions

module Data.Traversable.Unzip
  ( unzipEager
  , unzipEagerWith
  , unzipLazy
  , unzipLazyWith
  , unzipExtraLazy
  , unzipExtraLazyWith
  ) where

import Data.Traversable.Unzip.EagerPair
import Data.Traversable.Unzip.LazyPair
import Data.Traversable.Unzip.Coerce ((#.))
import Data.Biapplicative (traverseBia)

-- | A version of @"Data.List".'Data.List.unzip'@ that works for any
-- `Traversable` type. The container will be unzipped fully before anything is
-- returned.  In particular, this means that if any of the elements in the
-- container is bottom, then the result will be bottom.
--
-- @unzipEager [_|_] = _|_@
unzipEager :: Traversable t => t (a, b) -> (t a, t b)
unzipEager x = unzipEagerWith id x

-- | A version of 'unzipEager' that takes a function to use to produce the
-- pairs.
unzipEagerWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipEagerWith f = unEagerPair #. traverseBia (EagerPair #. f)

-- | A version of @"Data.List".'Data.List.unzip'@ that works for any
-- `Traversable` type. The results are as lazy as possible for the underlying
-- `Traversable` instance. The result is always lazy in the pairs. The result
-- pair is produced eagerly.
--
-- @unzipLazy _|_ = _|_@
--
-- @unzipLazy [_|_, _|_] = ([_|_, _|_], [_|_, _|_])@
unzipLazy :: Traversable t => t (a, b) -> (t a, t b)
unzipLazy x = unzipLazyWith id x

-- | A version of @unzipLazy@ that produces the result pair lazily.
--
-- @unzipExtraLazy _|_ = (_|_, _|_)@
--
-- @unzipExtraLazy [_|_, _|_] = ([_|_, _|_], [_|_, _|_])@
unzipExtraLazy :: Traversable t => t (a, b) -> (t a, t b)
unzipExtraLazy x = unzipExtraLazyWith id x

-- | A version of 'unzipLazy' that takes a function to use to produce the
-- pairs.
unzipLazyWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipLazyWith f = unLazyPair . traverseBia (LazyPair #. f)

-- | A version of 'unzipExtraLazy' that takes a function to use to produce the
-- pairs.
unzipExtraLazyWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipExtraLazyWith f = ensurePair . unLazyPair . traverseBia (LazyPair #. f)

-- Make a pair lazy
ensurePair :: (a, b) -> (a, b)
ensurePair ~(a, b) = (a, b)
