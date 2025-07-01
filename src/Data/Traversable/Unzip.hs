{-# LANGUAGE Trustworthy #-}  -- For safe coercions

-- | Suppose you have a container of pairs, @abs :: T (a, b)@, and you wish to
-- unzip it to get @as :: T a@ and @bs :: T b@.
--
-- One classic option is to use 'Data.Functor.unzip', which gives @(fmap fst
-- abs, fmap snd abs)@. This is nice and lazy, but it has a potential space
-- leak: the thunk representing the first component of the result keeps all the
-- second components of the argument live, and vice versa. So even after the
-- first component of the result becomes garbage, the first components of all
-- the elemnents of the argument will remain live, even though they can never
-- be used.
--
-- An alternative approach, to fix the above problem, is to fully traverse the
-- argument twice, before returning anything. The first traversal extracts (or
-- delays extraction of) all the first components of the argument, while the
-- second extracts (or delays extraction of) all the second components of the
-- argument. In the lazy case, this is potentially too eager: a lazy-spined
-- structure can generally be traversed incrementally, but this approach forces
-- it to be traversed all at once.  In the eager case, it's not too bad (and in
-- some cases may actually be best!), but it traverses the structure twice,
-- which can be expensive.
--
-- This module uses @"Data.Biapplicative".'traverseBia'@ to avoid these
-- problems: the structure is traversed lazily or eagerly, only once, and in
-- the lazy case great care is taken to avoid space leaks.
--
-- === Note
--
-- @"Data.List".'Data.List.unzip'@ has a nifty partially-lazy implementation
-- that inspects list conses only as needed, but pulls apart the pairs stored
-- in them eagerly. Unfortunately, there is no way to get such behavior for
-- general `Traversable` instances, essentially because we have no way to know
-- where to be lazy and where to be strict. It would be possible to write an
-- \"overly clever\" 'Traversable' version that just so happens to do that for
-- lists, but then it /wouldn't/ behave properly for, say, snoc lists. For more
-- intricate instances, its behavior could tend to the bizarre. As a result, we
-- don't include any such functions here.

module Data.Traversable.Unzip
  (
  -- * Eager unzips
    unzipEager , unzipEagerWith
  -- * Lazy unzips
  , unzipLazy , unzipLazyWith
  -- * Lazy unzips producing lazy pairs
  , unzipExtraLazy , unzipExtraLazyWith
  ) where

import Data.Traversable.Unzip.EagerPair
import Data.Traversable.Unzip.LazyPair
import Data.Traversable.Unzip.Coerce ((#.))
import Data.Biapplicative (traverseBia)

-- | An unzipping function that works for any `Traversable` type. The container
-- will be unzipped fully before anything is returned. In particular, this
-- means that if any of the elements in the container is bottom, then the
-- result will be bottom.
--
-- @unzipEager [⊥] = ⊥@
unzipEager :: Traversable t => t (a, b) -> (t a, t b)
unzipEager x = unzipEagerWith id x

-- | A version of 'unzipEager' that takes a function to use to produce the
-- pairs.
unzipEagerWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipEagerWith f = unEagerPair #. traverseBia (EagerPair #. f)

-- | An unzipping function that works for any `Traversable` type. The results
-- are as lazy as possible for the underlying `Traversable` instance. The
-- result is always lazy in the pairs. The result pair is produced eagerly.
--
-- @unzipLazy ⊥ = ⊥@
--
-- @unzipLazy [⊥, ⊥] = ([⊥, ⊥], [⊥, ⊥])@
--
-- @unzipLazy ((a, b) : ⊥ : (c, d) : ⊥) = (a : ⊥ : c : ⊥, b : ⊥ : d : ⊥)@
unzipLazy :: Traversable t => t (a, b) -> (t a, t b)
unzipLazy x = unzipLazyWith id x

-- | A version of @unzipLazy@ that produces the result pair lazily, exactly like
-- @"Data.Functor".'Data.Functor.unzip'@.
--
-- @unzipExtraLazy xs = "Data.Functor".'Data.Functor.unzip' xs@
--
-- @unzipExtraLazy ⊥ = (⊥, ⊥)@
--
-- @unzipExtraLazy [⊥, ⊥] = ([⊥, ⊥], [⊥, ⊥])@
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
