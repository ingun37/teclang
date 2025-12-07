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

data Index
  = IndexN {number :: Word}
  | IndexS {name :: String}
  | IndexU
  deriving (Show, Generic)

instance ToJSON Index where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Index

data TecAST
  = TecType {typeName :: String, index :: Index}
  | TecLayout {typeName :: String, children :: [TecAST]}
  | TecQuery {op :: String, left :: TecAST, right :: TecAST}
  | TecError
  deriving (Show, Generic)

instance ToJSON TecAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecAST

makeTecAST :: E.Exp l -> TecAST
makeTecAST (E.App _ (E.Con _ (E.UnQual _ (E.Ident _ conName))) exp) =
  let handle (E.List _ exps) = TecLayout conName (map makeTecAST exps)
      handle (E.Lit _ (E.Int _ val _)) = TecType conName (IndexN $ fromInteger val)
      handle (E.Lit _ (E.String _ val _)) = TecType conName (IndexS val)
      handle _ = undefined
   in handle exp
makeTecAST (E.Con _ (E.UnQual _ (E.Ident _ conName))) =
  TecType conName IndexU
makeTecAST (E.InfixApp _ l (E.QConOp _ (E.UnQual _ (E.Symbol _ op))) r) =
  TecQuery op (makeTecAST l) (makeTecAST r)
makeTecAST _ = TecError

makeExp :: TecAST -> E.Exp ()
makeExp tecAST = undefined

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
           in Right $ Parsed {reconstructedCode = reconstructed, ast = makeTecAST exp, rawASTShow = show exp}
        E.ParseFailed _ str ->
          Left str
