module TecAST where

import Control.Monad ((>=>))
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import TecClass
import TecData
import TecDecode
import TecEncode
import TecEnum
import TecError
import TecNode
import TecPnum
import TecQuery

class (Show a, Generic a, ToJSON a, FromJSON a) => TecAST a where
  decodeTecToCode :: a -> Either TecError String
  encodeCodeToTec :: String -> Either TecError a

mapWholeExpShow :: (Show l) => l -> Either TecError a -> Either TecError a
mapWholeExpShow x e = case e of
  (Left err) -> Left (TecErrorWithWholeExpShow err (show x))
  a -> a

tecError :: String -> Either TecError b
tecError str = Left $ TecError str

instance TecAST TecDataAST where
  decodeTecToCode ast = do
    e <- decodeTecDataAST ast
    let m = E.Module () Nothing [] [] [E.PatBind () (E.PVar () (E.Ident () "tecData")) (E.UnGuardedRhs () e) Nothing]
    return $ E.prettyPrint m
  encodeCodeToTec code =
    let result = E.parseModule code
     in case result of
          E.ParseOk (E.Module _ _ _ _ [E.PatBind _ _ (E.UnGuardedRhs _ e) _]) -> do
            -- tecError (show rhs)
            ast <- mapWholeExpShow e $ encodeTecDataAST e
            Right ast
          E.ParseOk x -> do
            Left $ TecErrorUnknownExp (show x)
          E.ParseFailed _ str ->
            tecError $ "Initial parsing failed:\n" ++ str

getDecls :: String -> Either TecError [E.Decl E.SrcSpanInfo]
getDecls code =
  let result = E.parseModule code
   in case result of
        E.ParseOk (E.Module _ Nothing [] [] decls) -> do
          Right decls
        E.ParseOk x -> do
          Left $ TecErrorUnknownExp (show x)
        E.ParseFailed _ str ->
          tecError $ "Initial parsing failed:\n" ++ str

setDecls :: [E.Decl ()] -> String
setDecls decls = let m = E.Module () Nothing [] [] decls in E.prettyPrint m

instance TecAST TecEnumAST where
  decodeTecToCode = fmap setDecls . decodeTecEnumAST
  encodeCodeToTec = getDecls >=> encodeTecEnumAST

instance TecAST TecClassAST where
  decodeTecToCode = fmap setDecls . decodeTecClassAST
  encodeCodeToTec = getDecls >=> encodeTecClassAST

instance TecAST TecNodeAST where
  decodeTecToCode = return . setDecls . decodeTecNodeAST
  encodeCodeToTec = getDecls >=> encodeTecNodeAST

instance TecAST TecPnumAST where
  decodeTecToCode = fmap setDecls . decodeTecPnumAST
  encodeCodeToTec = getDecls >=> encodeTecPnumAST

instance TecAST TecQuery where
  decodeTecToCode t = do
    e <- decodeTecQuery t
    return $ E.prettyPrint e
  encodeCodeToTec code = do
    let parseResult = E.parseExp code
    case parseResult of
      E.ParseOk e -> encodeTecQuery e
      x -> Left $ TecError (show x)