module Main (main) where
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as JP
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
  let jsonBytes = B.toStrict $ JP.encodePretty a
  putStrLn "---- Json encoded ----"
  putStrLn $ T.unpack $ E.decodeUtf8 jsonBytes
  putStrLn "---- Json decoded ----"
  let decoded = Json.decodeStrict jsonBytes :: Maybe TecAST
  print =<< maybe (fail "") pure decoded
  

main :: IO ()
main = do
  content <- readFile "test/samples/2.txt"
  let parsed = parseHaskellStr content
  mapM_ handle parsed
  putStrLn "---- End ----"