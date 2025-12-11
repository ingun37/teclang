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

_parseHaskell :: String -> ExceptT AppErr IO String
_parseHaskell code = do
  ast <- liftEither $ fmap MyLib.ast $ mapLeft TecErr $ MyLib.parseHaskellStr code
  let bytes = BS.toStrict $ J.encode ast
  let text = TE.decodeUtf8 bytes
  return $ T.unpack text

parseHaskell :: String -> IO String
parseHaskell x = do
  e <- runExceptT $ _parseHaskell x
  either (fail . show) return e
  
_makeHaskell :: String -> ExceptT AppErr IO String
_makeHaskell jsonStr = do
  let tecAst = J.decodeStrictText (T.pack jsonStr) :: Maybe MyLib.TecAST
  tecAst' <- liftEither $ maybe (Left $ ErrMsg "json decoding failed") Right tecAst
  liftEither $ mapLeft TecErr (MyLib.makeHaskellCode tecAst')

makeHaskell :: String -> IO String
makeHaskell x = do
  e <- runExceptT $ _makeHaskell x
  either (fail . show) return e