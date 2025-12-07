module Main (main) where

import qualified MyLib ()

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

foreign export ccall fibo :: Int -> Int

fibo n = n + 2