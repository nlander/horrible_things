{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Prelude hiding (foldr, take)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = go
  where
    go [] = z
    go (x:xs) = x `f` go xs

take :: Int -> [a] -> [a]
take n l = map fst $ foldr f [] $ zip l [0..]
  where
    f :: (a, Int) -> [(a, Int)] -> [(a, Int)]
    f tuple@(a, n') l =
      if n' < n then tuple : l else []

main :: IO ()
main = print $ take 2 [1,2,3,4]
