module Main (main) where
import qualified MyLib

code = "main :: IO()"

code2 = "Logo"

main :: IO ()
main = do
    content <- readFile "test/sample.hs"
    print content
    MyLib.parseHaskellStr content