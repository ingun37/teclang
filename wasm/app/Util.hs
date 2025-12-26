module Util where

import Control.Monad.Except
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import MyLib qualified

data AppErr = TecErr MyLib.TecError | ErrMsg String deriving (Show)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right a) = Right a

eD :: String -> Either MyLib.TecError (MyLib.Parsed MyLib.TecDataAST)
eD = MyLib.encodeCodeToTec

_encodeHaskellData :: String -> ExceptT AppErr IO String
_encodeHaskellData code = do
  ast <- liftEither $ fmap MyLib.ast $ mapLeft TecErr $ eD code
  let bytes = BS.toStrict $ J.encode ast
  let text = TE.decodeUtf8 bytes
  return $ T.unpack text

encodeHaskellData :: String -> IO String
encodeHaskellData x = do
  e <- runExceptT $ _encodeHaskellData x
  either (fail . show) return e

dD :: MyLib.TecDataAST -> Either MyLib.TecError String
dD = MyLib.decodeTecToCode

_decodeHaskellData :: String -> ExceptT AppErr IO String
_decodeHaskellData jsonStr = do
  let tecAst = J.decodeStrictText (T.pack jsonStr) :: Maybe MyLib.TecDataAST
  tecAst' <- liftEither $ maybe (Left $ ErrMsg "json decoding failed") Right tecAst
  liftEither $ mapLeft TecErr (dD tecAst')

decodeHaskellData :: String -> IO String
decodeHaskellData x = do
  e <- runExceptT $ _decodeHaskellData x
  either (fail . show) return e

formatHaskell :: String -> IO String
formatHaskell code = do
  code' <- runExceptT $ MyLib.formatHaskell code
  either (fail . show) return code'