{-# LANGUAGE BangPatterns #-}
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
-- === Note on lists
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
--
-- This function could be defined to operate in two passes thus:
--
-- @
-- unzipEager p
--   | 'Data.Tuple.MkSolo' as \<- 'traverse' (\\(a, _) -> 'Data.Tuple.MkSolo' a) $ p
--   , 'Data.Tuple.MkSolo' bs \<- 'traverse' (\\(_, b) -> 'Data.Tuple.MkSolo' b) $ p
-- = (as, bs)
-- @
--
-- Indeed, there are probably situations where the two-pass version will be
-- more efficient!  This is especially likely when the container is fairly
-- small and is of a type not in the @base@ package.
unzipEager :: Traversable t => t (a, b) -> (t a, t b)
unzipEager x = unzipEagerWith id x

-- | A version of 'unzipEager' that takes a function to use to produce the
-- pairs.
unzipEagerWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipEagerWith f = unEagerPair #. traverseBia (EagerPair #. f)

-- | An unzipping function that works for any `Traversable` type. The results
-- are as lazy as possible for the underlying `Traversable` instance. The
-- result is always lazy in the pairs contained in the argument. Both
-- components of the result are reduced to weak head normal form.
--
-- @unzipLazy p = case 'unzipExtraLazy' p of r\@(!_, !_) -> r@
--
-- @unzipLazy ⊥ = ⊥ -- (for strictly lawful instances)@
--
-- @unzipLazy [⊥, ⊥] = ([⊥, ⊥], [⊥, ⊥])@
--
-- @unzipLazy ((a, b) : ⊥ : (c, d) : ⊥) = (a : ⊥ : c : ⊥, b : ⊥ : d : ⊥)@
unzipLazy :: Traversable t => t (a, b) -> (t a, t b)
unzipLazy x = unzipLazyWith id x

-- | A version of @unzipLazy@ that produces the result pair lazily, exactly like
-- @"Data.Functor".'Data.Functor.unzip'@. We take special care to avoid leaking
-- inaccessible values.
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
-- See [Note: unzipLazy strictness, below]
unzipLazyWith f = strictifyPair . unLazyPair . traverseBia (LazyPair #. f)

-- Note: unzipLazy strictness
--
-- It might seem surprising that we force the components of the result.
-- Unfortunately, if we didn't do so, then @unzipLazy ⊥@ could be bottom or not
-- depending on details of the `Traversable` instance for the type in question
-- that users would almost certainly find surprising. For example, consider
--
--   newtype Foo a = Foo [a] deriving (Functor, Foldable, Traversable)
--
-- If we didn't force the components, then we'd have
--
--   unzipLazy (undefined @[Int]) = ⊥
--
--   unzipLazy (undefined @(Foo Int)) = (⊥, ⊥)
--
-- Why would this happen? Because there's a final bimap in the traversal to
-- apply the newtype constructors, and the Bifunctor instance for LazyPair
-- introduces laziness there!
--
-- Technically, we only need to force one of the components, but forcing both
-- seems a bit simpler and probably easier on the compiler.

-- | A version of 'unzipExtraLazy' that takes a function to use to produce the
-- pairs.
unzipExtraLazyWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipExtraLazyWith f = ensurePair . unLazyPair . traverseBia (LazyPair #. f)

-- Make a pair lazy
ensurePair :: (a, b) -> (a, b)
ensurePair ~(a, b) = (a, b)

-- Make a pair extra strict
strictifyPair :: (a, b) -> (a, b)
strictifyPair p@(!_, !_) = p
