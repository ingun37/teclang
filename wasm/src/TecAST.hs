module TecAST where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
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
  decodeTecToCode ast = fmap E.prettyPrint (decodeTecType ast)
  encodeCodeToTec code =
    let result = E.parseDecl ("data D = " ++ code)
     in case result of
          E.ParseOk e -> do
            ast <- mapWholeExpShow e $ encodeTecType e
            Right $ Parsed {ast = ast, rawAstShow = show e}
          E.ParseFailed _ str ->
            tecError $ "Initial parsing failed:\n" ++ str