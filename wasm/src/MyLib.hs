module MyLib (someFunc, parseHaskellStr) where

import qualified Language.Haskell.Exts as E

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall fib :: Int -> Int
fib n = n+1

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

parseHaskellStr :: String -> IO ()
parseHaskellStr code = do
    let result = E.parseFileContents code
    case result of
        E.ParseOk a -> do
            print "parse success"
            let aaa = E.prettyPrint a
            print aaa
        E.ParseFailed loc str -> print $ "noo" ++ str
