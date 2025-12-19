{-# LANGUAGE TemplateHaskell #-}

module MyLib
  ( encodeHaskellData,
    TecDataAST,
    Parsed (Parsed),
    decodeHaskellData,
    ast,
    rawAstShow,
    TecError (TecError, TecErrorUnknownExp, TecErrorWithWholeExpShow),
    tecError,
  )
where

-- import Data.ByteString qualified as BS
-- import Data.FileEmbed qualified as Embed
-- import Data.Text.Encoding qualified as TE

import Language.Haskell.Exts qualified as E
import TecDecode qualified as De
import TecEncode qualified as En
import TecTypes

mapWholeExpShow :: (Show l) => E.Exp l -> Either TecError a -> Either TecError a
mapWholeExpShow x e = case e of
  (Left err) -> Left (TecErrorWithWholeExpShow err (show x))
  a -> a

tecError :: String -> Either TecError b
tecError str = Left $ TecError str

encodeHaskellData :: String -> Either TecError (Parsed TecDataAST)
encodeHaskellData code =
  let result = E.parseExp code
   in case result of
        E.ParseOk e -> do
          ast <- mapWholeExpShow e $ En.encodeTecData e
          Right $ Parsed {ast = ast, rawAstShow = show e}
        E.ParseFailed _ str ->
          tecError $ "Initial parsing failed:\n" ++ str

decodeHaskellData :: TecDataAST -> Either TecError String
decodeHaskellData ast = fmap E.prettyPrint (De.decodeTecData ast)
