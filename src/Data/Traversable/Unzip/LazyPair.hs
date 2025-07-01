{-# LANGUAGE DerivingStrategies #-}
module Data.Traversable.Unzip.LazyPair (LazyPair (..)) where
import Data.Biapplicative (Bifunctor (..), Biapplicative (..))

-- | A pair with a lazier 'Functor' instance (we don't actually use), and with
-- 'Bifunctor' and 'Biapplicative' instances designed not to leak memory.
newtype LazyPair a b = LazyPair { unLazyPair :: (a, b) }

-- Getting the thunks we want when we want them is quite fragile. I found it
-- helpful to inspect the Core for unzipping Maps; those have enough strictness
-- to make GHC want to do bad things that will leak memory. It may be possible
-- to improve it, but this is the most robust approach I've found so far.

instance Functor (LazyPair a) where
  fmap f (LazyPair ab) = combine a b
    where
      ~(a, b) = ab
      combine x y = LazyPair (x, f y)
      {-# NOINLINE combine #-}

instance Bifunctor LazyPair where
  bimap f g (LazyPair ab) = combine a b
    where
      ~(a, b) = ab
      combine p q = LazyPair (f p, g q)
      {-# NOINLINE combine #-}

instance Biapplicative LazyPair where
  bipure x y = LazyPair (x, y)
  biliftA2 f g (LazyPair x1y1) (LazyPair x2y2) = combine x1 x2 y1 y2
    where
      ~(x1, y1) = x1y1
      ~(x2, y2) = x2y2
      -- I worked out this "combine" trick for Data.List.transpose.
      -- By marking the combine function NOINLINE, we ensure that
      -- all four selector thunks are constructed up front. In particular,
      -- we don't let GHC do something like
      --
      -- (let {~(x1, _) = x1y1; ~(x2, _) = x2y2} in f x1 x2,
      --  let {~(_, y1) = x1y1; ~(_, y2) = x2y2} in g y1 y2)
      combine p q r s = LazyPair (f p q, g r s)
      {-# NOINLINE combine #-}
