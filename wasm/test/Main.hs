module Main (main) where

import MyLib
import Text.Pretty.Simple qualified as Simple

handle :: Parsed -> IO ()
handle (Parsed {reconstructedCode = c, ast = a}) = do
  putStrLn "---- Raw AST ----"
  Simple.pPrintString =<< readFile "test/output.txt"
  putStrLn "---- Final AST ----"
  Simple.pPrint a
  putStrLn "---- Reconstructed Code ----"
  putStrLn c

main :: IO ()
main = do
  content <- readFile "test/sample.txt"
  let parsed = parseHaskellStr content
  putStrLn "---- Raw AST ----"
  Simple.pPrintString =<< readFile "test/output.txt"
  mapM_ handle parsed
  putStrLn "---- End ----"