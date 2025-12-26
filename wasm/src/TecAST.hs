module TecAST where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import TecData
import TecDecode
import TecEncode
import TecError
import TecTypes

class (Show a, Generic a, ToJSON a, FromJSON a) => TecAST a where
  decodeTecToCode :: a -> Either TecError String
  encodeCodeToTec :: String -> Either TecError (Parsed a)

mapWholeExpShow :: (Show l) => l -> Either TecError a -> Either TecError a
mapWholeExpShow x e = case e of
  (Left err) -> Left (TecErrorWithWholeExpShow err (show x))
  a -> a

tecError :: String -> Either TecError b
tecError str = Left $ TecError str

instance TecAST TecDataAST where
  decodeTecToCode ast = do
    e <- decodeTecData ast
    let m = E.Module () Nothing [] [] [E.PatBind () (E.PVar () (E.Ident () "tecData")) (E.UnGuardedRhs () e) Nothing]
    return $ E.prettyPrint m
  encodeCodeToTec code =
    let result = E.parseModule code
     in case result of
          E.ParseOk (E.Module _ _ _ _ [E.PatBind _ _ (E.UnGuardedRhs _ e) _]) -> do
            -- tecError (show rhs)
            ast <- mapWholeExpShow e $ encodeTecData e
            Right $ Parsed {ast = ast, rawAstShow = show e}
          E.ParseOk x -> do
            Left $ TecErrorUnknownExp (show x)
          E.ParseFailed _ str ->
            tecError $ "Initial parsing failed:\n" ++ str

instance TecAST TecTypeAST where
  decodeTecToCode ast = do
    decls <- decodeTecType ast
    let m = E.Module () Nothing [] [] decls
    return $ E.prettyPrint m
  encodeCodeToTec code =
    let result = E.parseModule code
     in case result of
          E.ParseOk (E.Module _ Nothing [] [] decls) -> do
            case decls of
              [decl] -> do
                let f e = mapWholeExpShow e $ encodeTecType e
                ast <- f decl
                Right $ Parsed {ast = ast, rawAstShow = show ast}
              [] -> Left $ TecError "decls is empty"
              _ -> Left $ TecErrorUnknownExpWithMessage (show decls) "More than one decl is given"
          E.ParseOk x -> do
            Left $ TecErrorUnknownExp (show x)
          E.ParseFailed _ str ->
            tecError $ "Initial parsing failed:\n" ++ str