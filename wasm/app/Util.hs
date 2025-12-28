module Util where

import Control.Monad.Except
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Functor.Const
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import MyLib qualified

data Err = LibErr MyLib.TecError | AppErr String deriving (Show)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right a) = Right a

eD :: String -> Either MyLib.TecError (MyLib.Parsed MyLib.TecDataAST)
eD = MyLib.encodeCodeToTec

_encodeHaskellData :: String -> ExceptT Err IO String
_encodeHaskellData code = do
  ast <- liftEither $ fmap MyLib.ast $ mapLeft LibErr $ eD code
  let bytes = BS.toStrict $ J.encode ast
  let text = TE.decodeUtf8 bytes
  return $ T.unpack text

encodeHaskellData :: String -> IO String
encodeHaskellData x = do
  e <- runExceptT $ _encodeHaskellData x
  either (fail . show) return e

failIfLeft :: (Show l) => Either l r -> IO r
failIfLeft = either (fail . show) return

type Sig a = (MyLib.TecAST a) => Const String a -> String
sigType :: Sig MyLib.TecTypeAST
sigType = getConst
sigData :: Sig MyLib.TecDataAST
sigData = getConst

decodeHaskell :: forall a. (MyLib.TecAST a, J.FromJSON a) => String -> IO (Const String a)
decodeHaskell jsonStr = do
  let m = J.decodeStrictText (T.pack jsonStr) :: Maybe a
  let e = maybe (Left $ AppErr "json decoding failed") Right m
  let e' = (mapLeft LibErr . MyLib.decodeTecToCode) =<< e
  failIfLeft (Const <$> e')

decodeHaskellData :: String -> IO String
decodeHaskellData = fmap sigData . decodeHaskell

decodeHaskellType :: String -> IO String
decodeHaskellType = fmap sigType . decodeHaskell

formatHaskell :: String -> IO String
formatHaskell code = do
  code' <- runExceptT $ MyLib.formatHaskell code
  either (fail . show) return code'