module Main (main) where

import Data.Aeson.Encode.Pretty qualified as J
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import MyLib
import Text.Pretty.Simple qualified as Simple

handle :: Parsed -> IO ()
handle (Parsed {reconstructedCode = c, ast = a, rawASTShow = r}) = do
  putStrLn "---- Raw ----"
  Simple.pPrintString r
  putStrLn "---- Final AST ----"
  Simple.pPrint a
  putStrLn "---- Reconstructed Code ----"
  putStrLn c
  putStrLn "---- Json AST ----"
  putStrLn $ T.unpack $ E.decodeUtf8 (B.toStrict $ J.encodePretty a)

main :: IO ()
main = do
  content <- readFile "test/samples/2.txt"
  let parsed = parseHaskellStr content
  mapM_ handle parsed
  putStrLn "---- End ----"