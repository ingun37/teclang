module Util
  ( encodeHaskellData,
    encodeHaskellType,
    decodeHaskellData,
    decodeHaskellType,
    formatHaskell,
  )
where

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

failIfLeft :: (Show l) => Either l r -> IO r
failIfLeft = either (fail . show) return

type Sig a = (MyLib.TecAST a) => Const String a -> String

sigType :: Sig MyLib.TecEnumAST
sigType = getConst

sigData :: Sig MyLib.TecDataAST
sigData = getConst

encodeHaskell :: forall a. (MyLib.TecAST a, J.ToJSON a) => String -> IO (Const String a)
encodeHaskell code = do
  ast <- failIfLeft $ mapLeft LibErr $ MyLib.encodeCodeToTec code :: IO a
  return $ Const $ T.unpack $ TE.decodeUtf8 $ BS.toStrict $ J.encode ast

encodeHaskellData :: String -> IO String
encodeHaskellData = fmap sigData . encodeHaskell

encodeHaskellType :: String -> IO String
encodeHaskellType = fmap sigType . encodeHaskell

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