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

_parseHaskellData :: String -> ExceptT AppErr IO String
_parseHaskellData code = do
  ast <- liftEither $ fmap MyLib.ast $ mapLeft TecErr $ MyLib.parseHaskellData code
  let bytes = BS.toStrict $ J.encode ast
  let text = TE.decodeUtf8 bytes
  return $ T.unpack text

parseHaskellData :: String -> IO String
parseHaskellData x = do
  e <- runExceptT $ _parseHaskellData x
  either (fail . show) return e
  
_makeHaskellData :: String -> ExceptT AppErr IO String
_makeHaskellData jsonStr = do
  let tecAst = J.decodeStrictText (T.pack jsonStr) :: Maybe MyLib.TecDataAST
  tecAst' <- liftEither $ maybe (Left $ ErrMsg "json decoding failed") Right tecAst
  liftEither $ mapLeft TecErr (MyLib.makeHaskellData tecAst')

makeHaskellData :: String -> IO String
makeHaskellData x = do
  e <- runExceptT $ _makeHaskellData x
  either (fail . show) return e