module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall fib :: Int -> Int
fib n = n+1