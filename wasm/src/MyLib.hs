{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc, parseHaskellStr, TecAST) where

import Data.ByteString qualified as BS
import Data.FileEmbed qualified as Embed
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.Haskell.Exts qualified as E
import Language.Haskell.TH qualified as TS
import Language.Haskell.TH.Syntax qualified as TS
import System.FilePath qualified as FP

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall fib :: Int -> Int

fib n = n + 1

data TecAST = TecType String | TecLayout String [TecAST] deriving (Show)

makeTecASTName :: E.Name l -> TecAST
makeTecASTName (E.Ident _ name) = TecType name

makeTecASTQName :: E.QName l -> TecAST
makeTecASTQName (E.UnQual _ name) = makeTecASTName name
makeTecASTQName _ = undefined

makeTecASTExp :: E.Exp l -> TecAST
makeTecASTExp (E.Con _ qname) = makeTecASTQName qname
makeTecASTExp (E.App _ (E.Con _ (E.UnQual _ (E.Ident _ layoutName))) (E.List _ exps)) = TecLayout layoutName (map makeTecASTExp exps)
makeTecASTExp _ = undefined

makeTecASTDecl :: E.Decl l -> TecAST
makeTecASTDecl (E.PatBind _ _ (E.UnGuardedRhs _ exp) _) = makeTecASTExp exp
makeTecASTDecl _ = undefined

makeTecASTsModule :: E.Module l -> [TecAST]
makeTecASTsModule (E.Module _ _ _ _ decls) =
  let patBindings = [x | x@(E.PatBind {}) <- decls]
   in map makeTecASTDecl patBindings
makeTecASTsModule _ = undefined

tecCode :: BS.ByteString
tecCode = $(Embed.embedFile "src/TecType.hs")

parseHaskellStr :: String -> IO (String, [TecAST])
parseHaskellStr code = do
  let tecCodeTxt = TE.decodeUtf8 tecCode
  let result = E.parseFileContents (T.unpack tecCodeTxt ++ "\n" ++ code)
  case result of
    E.ParseOk a -> do
      writeFile "test/output.txt" (show a)
      let reconstructed = E.prettyPrint a
      return (reconstructed, makeTecASTsModule a)
    E.ParseFailed loc str -> do
      print $ "noo" ++ str
      undefined
