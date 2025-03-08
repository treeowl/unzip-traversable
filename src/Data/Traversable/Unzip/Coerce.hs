module Data.Traversable.Unzip.Coerce ((#.)) where
import Data.Coerce (Coercible, coerce)

infixr 9 #.
(#.) :: Coercible b c => p b c -> (a -> b) -> a -> c
(#.) _ = coerce
