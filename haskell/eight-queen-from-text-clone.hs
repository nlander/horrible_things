module Main where

import Control.Concurrent.MVar
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.IORef

boardSize :: Int
boardSize = 8

isplaceok :: [Int] -> Int -> Int -> MVar Bool -> IO ()
isplaceok a n c result = do
  for_ [1..(n-1)]
    (\i -> if a !! (i - 1) == c ||
              a !! (i - 1) - i == c - n ||
              a !! (i - 1) + i == c + n
           then putMVar result False
           else pure ()
    )
  _ <- tryPutMVar result True
  pure ()

printsolution :: [Int] -> IO ()
printsolution a = do
  for_ [1..boardSize] (\i -> do
      for_ [1..boardSize] (\j -> do
          let c = bool "-" "X" $ a !! (i - 1) == j
          putStr $ c ++ " "
        )
      putStr "\n"
    )
  putStr "\n"

addqueen :: MVar Bool -> IORef [Int] -> Int -> IO ()
addqueen placeokVar aRef n =
  if n > boardSize
  then readIORef aRef >>= printsolution
  else for_ [1..boardSize] (\c -> do
           a <- readIORef aRef
           isplaceok a n c placeokVar
           placeok <- takeMVar placeokVar
           if placeok then do
             modifyIORef aRef $ set n c
             addqueen placeokVar aRef (n + 1)
           else pure ()
         )
  where
    set :: Int -> Int -> [Int] -> [Int]
    set i v l = take (i - 1) l ++ [v] ++ drop i l

main :: IO ()
main = do
  placeokVar <- newEmptyMVar
  aRef <- newIORef []
  addqueen placeokVar aRef 1
