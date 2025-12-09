module Main (main) where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as JP
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import MyLib
import Text.Pretty.Simple qualified as Simple

data TestErr = ErrStr String | ErrTec TecError deriving (Show)

type ErrM = ExceptT TestErr IO

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right a) = Right a

testE :: String -> ErrM ()
testE code = do
  lift $ putStrLn "============== TESTING =============="
  lift $ putStrLn "---- Original Code ----"
  lift $ putStrLn code
  Parsed {ast} <- liftEither $ mapLeft ErrTec $ parseHaskellStr code
  lift $ putStrLn "---- Final AST ----"
  lift $ Simple.pPrint ast
  lift $ putStrLn "---- Json AST ----"
  let jsonBytes = B.toStrict $ JP.encodePretty ast
  lift $ putStrLn "---- Json encoded ----"
  lift $ putStrLn $ T.unpack $ E.decodeUtf8 jsonBytes
  let decodedMaybe = Json.decodeStrict jsonBytes :: Maybe TecAST
  let decodedEither = maybe (Left $ ErrStr "decode fail") Right decodedMaybe
  tecAST <- liftEither decodedEither
  lift $ putStrLn "---- Reconstructed Code ----"
  reconstructedCode <- liftEither $ mapLeft ErrTec $ makeHaskellCode tecAST
  lift $ putStrLn reconstructedCode
  if reconstructedCode == code
    then
      lift $ putStrLn "Success!!"
    else
      liftEither $ Left $ ErrStr "code and reconstructed code doesnt' match"

main :: IO ()
main = do
  let a = traverse testE testData
  b <- runExceptT a
  case b of
    (Left (ErrTec (TecErrorUnknownExp rawAstShow rawWholeAstShow))) -> do
      putStrLn "\n\n---- Unknown AST show ----\n\n"
      Simple.pPrintString rawAstShow
      putStrLn "\n\n---- Entire AST show ----\n\n"
      Simple.pPrintString rawWholeAstShow
    e -> print e

testData :: [String]
testData =
  [ "Logo",
    "HStack []",
    "Colorway 0",
    "Pantone \"red\"",
    "VStack [HStack [Logo], Colorway 42]",
    "Logo :- Logo",
    "Logo :- Logo :- Pantone \"green\"",
    "Colorways [1 ..]",
    "Colorways [1 .. 3]",
    "Render Front"
    -- "Renders [Front .. Right]"
    -- "Render 0 Front"
  ]