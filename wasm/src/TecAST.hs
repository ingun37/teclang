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
import TecEnum

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
    e <- decodeTecData ast
    let m = E.Module () Nothing [] [] [E.PatBind () (E.PVar () (E.Ident () "tecData")) (E.UnGuardedRhs () e) Nothing]
    return $ E.prettyPrint m
  encodeCodeToTec code =
    let result = E.parseModule code
     in case result of
          E.ParseOk (E.Module _ _ _ _ [E.PatBind _ _ (E.UnGuardedRhs _ e) _]) -> do
            -- tecError (show rhs)
            ast <- mapWholeExpShow e $ encodeTecData e
            Right ast
          E.ParseOk x -> do
            Left $ TecErrorUnknownExp (show x)
          E.ParseFailed _ str ->
            tecError $ "Initial parsing failed:\n" ++ str

instance TecAST TecEnumAST where
  decodeTecToCode ast = do
    decls <- decodeTecEnumAST ast
    let m = E.Module () Nothing [] [] decls
    return $ E.prettyPrint m
  encodeCodeToTec code =
    let result = E.parseModule code
     in case result of
          E.ParseOk (E.Module _ Nothing [] [] decls) -> do
            ast <- encodeTecEnumAST decls
            Right ast
          E.ParseOk x -> do
            Left $ TecErrorUnknownExp (show x)
          E.ParseFailed _ str ->
            tecError $ "Initial parsing failed:\n" ++ str