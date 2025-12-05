{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc, parseHaskellStr, TecAST, Parsed (Parsed), reconstructedCode, ast) where

import Data.ByteString qualified as BS
import Data.FileEmbed qualified as Embed
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.Haskell.Exts qualified as E

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall fib :: Int -> Int

fib n = n + 1

data TecAST = TecType String | TecLayout String [TecAST] deriving (Show)

makeTecASTName :: E.Name l -> TecAST
makeTecASTName (E.Ident _ name) = TecType name
makeTecASTName _ = undefined

makeTecASTQName :: E.QName l -> TecAST
makeTecASTQName (E.UnQual _ name) = makeTecASTName name
makeTecASTQName _ = undefined

makeTecASTExp :: E.Exp l -> TecAST
makeTecASTExp (E.Con _ qname) = makeTecASTQName qname
makeTecASTExp (E.App _ (E.Con _ (E.UnQual _ (E.Ident _ layoutName))) (E.List _ exps)) = TecLayout layoutName (map makeTecASTExp exps)
makeTecASTExp _ = undefined

makeTecASTRhs :: E.Rhs l -> TecAST
makeTecASTRhs (E.UnGuardedRhs _ exp) = makeTecASTExp exp
makeTecASTRhs _ = undefined

extractDocExp :: E.Module l -> E.Rhs l
extractDocExp (E.Module _ _ _ _ decls) = head [rhs | x@(E.PatBind _ _ rhs _) <- decls]
extractDocExp _ = undefined

tecCode :: BS.ByteString
tecCode = $(Embed.embedFile "src/TecSyntax.hs")

data Parsed = Parsed {reconstructedCode :: String, ast :: TecAST}

parseHaskellStr :: String -> Either String Parsed
parseHaskellStr code =
  let tecCodeTxt = TE.decodeUtf8 tecCode
      result = E.parseFileContents (T.unpack tecCodeTxt ++ "\ndoc = " ++ code)
   in case result of
        E.ParseOk a ->
          let rhs = extractDocExp a
              reconstructed = E.prettyPrint rhs
           in Right $ Parsed {reconstructedCode = reconstructed, ast = makeTecASTRhs rhs}
        E.ParseFailed _ str ->
          Left str
