module Main (main) where
import Data.Traversable.Unzip
import Data.Bifunctor (bimap)

main :: IO ()
main = do
  print . bimap length length $ unzipLazy [undefined :: (Int, Int), undefined, undefined]
  print . bimap (take 3) (take 3) $ unzipLazy ((1 :: Int, 'a') : (2, 'b') : (3, 'c') : undefined)
