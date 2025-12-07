module Main (main) where

import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as JP
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import MyLib
import Text.Pretty.Simple qualified as Simple
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Class

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
        -- putStrLn $ makeHaskellCode decoded
  mapM_ testAst decodedMaybe

type TecErrM = ExceptT TecError IO

testE :: String -> TecErrM ()
testE code = do
  lift $ putStrLn "============== TESTING =============="
  lift $ putStrLn "---- Original Code ----"
  lift $ putStrLn code
  Parsed {ast, rawASTShow} <- liftEither $ parseHaskellStr code
  lift $ putStrLn "---- Raw ----"
  lift $ Simple.pPrintString rawASTShow
  lift $ putStrLn "---- Final AST ----"
  lift $ Simple.pPrint ast
  lift $ putStrLn "---- Json AST ----"
  let jsonBytes = B.toStrict $ JP.encodePretty ast
  lift $ putStrLn "---- Json encoded ----"
  lift $ putStrLn $ T.unpack $ E.decodeUtf8 jsonBytes
  let decodedMaybe = Json.decodeStrict jsonBytes :: Maybe TecAST
  tecAST <- liftEither $ maybe (tecError "decode fail") Right decodedMaybe
  lift $ putStrLn "---- Reconstructed Code ----"
  reconstructedCode <- liftEither $ makeHaskellCode tecAST
  lift $ putStrLn reconstructedCode

main :: IO ()
main = do
  let a = traverse testE testData
  b <- runExceptT a
  print b

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