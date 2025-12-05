{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc, parseHaskellStr) where

import Data.ByteString qualified as BS
import Data.FileEmbed qualified as Embed
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.Haskell.Exts qualified as E
import Language.Haskell.TH qualified as TS
import Language.Haskell.TH.Syntax qualified as TS
import System.FilePath qualified as FP
import qualified System.FilePath as FP

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall fib :: Int -> Int

fib n = n + 1

data TecAST = TecType String | TecLayout [TecAST]

makeTecASTName :: E.Name l -> TecAST
makeTecASTName (E.Ident _ name) = TecType name

makeTecASTQName :: E.QName l -> TecAST
makeTecASTQName (E.UnQual _ name) = makeTecASTName name
makeTecASTQName _ = undefined

makeTecASTExp :: E.Exp l -> TecAST
makeTecASTExp (E.Con _ qname) = makeTecASTQName qname
makeTecASTExp _ = undefined

makeTecASTDecl :: E.Decl l -> TecAST
makeTecASTDecl (E.PatBind _ _ (E.UnGuardedRhs _ exp) _) = makeTecASTExp exp
makeTecASTDecl _ = undefined

tecCode :: BS.ByteString
tecCode = $(Embed.embedFile "src/TecType.hs")

parseHaskellStr :: String -> IO ()
parseHaskellStr code = do
  let tecCodeTxt = TE.decodeUtf8 tecCode
  print "--------"
  print tecCodeTxt
  let result = E.parseFileContents (T.unpack tecCodeTxt ++ "\n" ++ code)
  case result of
    E.ParseOk a -> do
      print "parse success"
      let aaa = E.prettyPrint a
      print aaa
    E.ParseFailed loc str -> print $ "noo" ++ str
