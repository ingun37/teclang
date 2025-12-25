module Main (main) where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as JP
import Data.ByteString qualified as B
import Data.Foldable
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO (hPutStrLn)
import MyLib
import System.IO qualified as IO
import Text.Pretty.Simple qualified as Simple

data TestErr = ErrStr String | ErrTec TecError deriving (Show)

type ErrM = ExceptT TestErr IO

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right a) = Right a

testE :: forall a. (TecAST a) => IO.Handle -> String -> ErrM a
testE logHandle code = do
  lift $ putStrLn "============== TESTING =============="
  lift $ putStrLn "---- Original Code ----"
  lift $ putStrLn code
  lift $ hPutStrLn logHandle (T.pack "---- Original Code ----")
  lift $ hPutStrLn logHandle (T.pack code)
  Parsed {ast, rawAstShow} <- liftEither $ mapLeft ErrTec $ encodeCodeToTec code
  lift $ hPutStrLn logHandle (T.pack "---- Raw AST ----")
  Simple.pHPrintString logHandle rawAstShow
  lift $ putStrLn "---- Final AST ----"
  lift $ Simple.pPrint ast
  lift $ putStrLn "---- Json AST ----"
  let jsonBytes = B.toStrict $ JP.encodePretty ast
  lift $ putStrLn "---- Json encoded ----"
  lift $ putStrLn $ T.unpack $ E.decodeUtf8 jsonBytes
  let decodedMaybe = Json.decodeStrict jsonBytes :: Maybe TecDataAST
  let decodedEither = maybe (Left $ ErrStr "decode fail") Right decodedMaybe
  tecAST <- liftEither decodedEither
  lift $ putStrLn "---- Reconstructed Code ----"
  reconstructedCode <- liftEither $ mapLeft ErrTec $ decodeTecToCode tecAST
  lift $ putStrLn reconstructedCode
  if reconstructedCode == code
    then
      lift $ putStrLn "Success!!"
    else
      liftEither $ Left $ ErrStr "code and reconstructed code doesnt' match"
  return ast

failIfLeft :: Either TestErr a -> IO a
failIfLeft b =
  case b of
    (Left (ErrTec (TecErrorWithWholeExpShow initialErr rawWholeAstShow))) -> do
      putStrLn "\n\n---- Initial error ----\n\n"
      case initialErr of
        (TecErrorUnknownExp e) -> Simple.pPrintString e
        _ -> Simple.pPrint initialErr
      putStrLn "\n\n---- Entire AST show ----\n\n"
      Simple.pPrintString rawWholeAstShow
      fail "test failed"
    (Left e) -> do
      Simple.pPrint e
      fail "test failed"
    (Right e) -> return e

testIO :: (TecAST a) => [String] -> FilePath -> IO [a]
testIO codes logFilePath = do
  logHandle <- IO.openFile logFilePath IO.WriteMode
  let a = traverse (testE logHandle) codes
  b <- runExceptT a
  IO.hClose logHandle
  failIfLeft b

testData :: [String]
testData =
  map
    ("tecData = " ++)
    [ "Logo",
      "HStack [Logo]",
      "Colorway 0",
      "Pantone \"red\"",
      "VStack [HStack [Logo], Colorway 42]",
      "Logo :- Logo",
      "Logo :- Logo :- Pantone \"green\"",
      "Colorways [1 ..]",
      "Colorways [1 .. 3]",
      "Render 0 Front",
      "Colorways [0 ..] :> Fabric \"*\"",
      "Colorways [0]",
      "HStack Logo (Render 0 Front)",
      "Render [Front .. Right]",
      "let a = 0 in a",
      "let in 0"
    ]

testType :: [String]
testType =
  map
    ("data TecType = " ++)
    [ "A | B",
      "A String"
    ]

testFormatUnit :: IO.Handle -> String -> IO ()
testFormatUnit h s = do
  e <- runExceptT $ formatHaskell s
  formatted <- failIfLeft (mapLeft ErrTec e)
  hPutStrLn h (T.pack "---- Formated ----")
  hPutStrLn h (T.pack formatted)

formatTestData :: [String]
formatTestData =
  [ "data Letters =   A | B",
    "data Letters = \n    A    | B"
  ]

formatTest :: FilePath -> IO ()
formatTest logFilePath = do
  logHandle <- IO.openFile logFilePath IO.WriteMode
  traverse_ (testFormatUnit logHandle) formatTestData

main :: IO ()
main = do
  formatTest "out-format.log"
  _ <- testIO testData "out-data.log" :: IO [TecDataAST]
  _ <- testIO testType "out-type.log" :: IO [TecTypeAST]
  return ()