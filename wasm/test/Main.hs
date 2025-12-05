module Main (main) where
import qualified MyLib

import Text.Pretty.Simple qualified as Simple

code = "main :: IO()"

code2 = "Logo"

main :: IO ()
main = do
    content <- readFile "test/sample.txt"
    (reconstructedCode, asts) <- MyLib.parseHaskellStr content
    putStrLn "---- Raw AST ----"
    Simple.pPrintString =<< readFile "test/output.txt"
    putStrLn "---- Final AST ----"
    Simple.pPrint asts
    putStrLn "---- Reconstructed Code ----"
    putStrLn reconstructedCode