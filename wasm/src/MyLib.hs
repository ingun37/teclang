{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc, parseHaskellStr, TecAST, Parsed (Parsed), reconstructedCode, ast, rawASTShow) where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as Embed
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import TecSyntax qualified as TS

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall fib :: Int -> Int

fib n = n + 1

data TecAST
  = TecType TS.TecType
  | TecLayout String [TecAST]
  | TecError
  deriving (Show, Generic)

instance ToJSON TecAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecAST

-- makeTecASTName :: E.Name l -> TecAST
-- makeTecASTName (E.Ident _ name) = TecType name
-- makeTecASTName _ = TecError

-- makeTecASTQName :: E.QName l -> TecAST
-- makeTecASTQName (E.UnQual _ name) = makeTecASTName name
-- makeTecASTQName _ = TecError

makeTecASTExp :: E.Exp l -> TecAST
makeTecASTExp (E.App _ (E.Con _ (E.UnQual _ (E.Ident _ conName))) exp) = case conName of
  "Colorway" -> case exp of
    (E.Lit _ (E.Int _ val _)) -> TecType (TS.Colorway (fromInteger val))
    _ -> TecError
  _ -> TecError
makeTecASTExp _ = TecError

extractDocExp :: E.Module l -> E.Exp l
extractDocExp (E.Module _ _ _ _ decls) = head [exp | x@(E.PatBind _ _ ((E.UnGuardedRhs _ exp)) _) <- decls]
extractDocExp _ = undefined

tecCode :: BS.ByteString
tecCode = $(Embed.embedFile "src/TecSyntax.hs")

data Parsed = Parsed
  { reconstructedCode :: String,
    ast :: TecAST,
    rawASTShow :: String
  }

parseHaskellStr :: String -> Either String Parsed
parseHaskellStr code =
  let tecCodeTxt = TE.decodeUtf8 tecCode
      result = E.parseFileContents (T.unpack tecCodeTxt ++ "\ndoc = " ++ code)
   in case result of
        E.ParseOk a ->
          let exp = extractDocExp a
              reconstructed = E.prettyPrint exp
           in Right $ Parsed {reconstructedCode = reconstructed, ast = makeTecASTExp exp, rawASTShow = show exp}
        E.ParseFailed _ str ->
          Left str
