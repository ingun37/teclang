module Main (main) where

import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as JP
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import MyLib
import System.Directory qualified as D
import Text.Pretty.Simple qualified as Simple

handle :: Parsed -> IO ()
handle (Parsed {ast = a, rawASTShow = r}) = do
  putStrLn "---- Raw ----"
  Simple.pPrintString r
  putStrLn "---- Final AST ----"
  Simple.pPrint a
  putStrLn "---- Json AST ----"
  let jsonBytes = B.toStrict $ JP.encodePretty a
  putStrLn "---- Json encoded ----"
  putStrLn $ T.unpack $ E.decodeUtf8 jsonBytes
  putStrLn "---- Json decoded ----"
  let decodedMaybe = Json.decodeStrict jsonBytes :: Maybe TecAST
  let testAst decoded = do
        print decoded
        putStrLn "---- Reconstructed Code ----"
        putStrLn $ makeHaskellCode decoded
  mapM_ testAst decodedMaybe

testEntry :: String -> IO ()
testEntry code = do
  putStrLn "============== TESTING =============="
  putStrLn "---- Original Code ----"
  putStrLn code
  let parsed = parseHaskellStr code
  mapM_ handle parsed

main :: IO ()
main = do
  mapM_ testEntry testData

testData :: [String]
testData =
  [ "Logo",
    "HStack []",
    "Colorway 0",
    "Pantone \"red\"",
    "VStack [HStack [Logo], Colorway 42]",
    "Logo :- Logo",
    "Logo :- Logo :- Pantone \"green\""
  ]