module Data.Traversable.Unzip
  ( unzipEager
  , unzipEagerWith
  , unzipLazy
  , unzipLazyWith
  ) where
import Data.Traversable.Unzip.EagerPair
import Data.Traversable.Unzip.LazyPair
import Data.Traversable.Unzip.Coerce ((#.))
import Data.Biapplicative (traverseBia)

-- | A version of @"Data.List".'Data.List.unzip'@ that works for any
-- `Traversable` type. The container will be unzipped fully before anything is
-- returned.  In particular, this means that if any of the elements in the
-- container is bottom, then the result will be bottom.
unzipEager :: Traversable t => t (a, b) -> (t a, t b)
unzipEager x = unzipEagerWith id x

-- | A version of 'unzipEager' that takes a function to use to produce the
-- pairs.
unzipEagerWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipEagerWith f = unEagerPair #. traverseBia (EagerPair #. f)

-- | A version of @"Data.List".'Data.List.unzip'@ that works for any
-- `Traversable` type. The container will be unzipped fully before anything is
-- returned.  In particular, this means that if any of the elements in the
-- container is bottom, then the result will be bottom.
unzipLazy :: Traversable t => t (a, b) -> (t a, t b)
unzipLazy x = unzipLazyWith id x

-- | A version of 'unzipEager' that takes a function to use to produce the
-- pairs.
unzipLazyWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipLazyWith f = ensurePair . unLazyPair . traverseBia (LazyPair #. f)

-- Make a pair lazy
ensurePair :: (a, b) -> (a, b)
ensurePair ~(a, b) = (a, b)
