module TecEncode where

import Data.Functor ((<&>))
import Language.Haskell.Exts qualified as E
import TecTypes
import TecError
import Data.Map qualified as Map
encodeDecl :: (Show l) => E.Decl l -> Either TecError (String, E.Exp l)
encodeDecl (E.PatBind _ (E.PVar _ (E.Ident _ name)) (E.UnGuardedRhs _ expr) _) = Right $ (name, expr)
encodeDecl x = Left $ TecErrorUnknownExp (show x)

encodeTecData :: (Show l) => E.Exp l -> Either TecError TecDataAST
encodeTecData (E.Var _ (E.UnQual _ (E.Ident _ name))) = return $ TecVar name
encodeTecData (E.Let _ (E.BDecls _ bindings) expression) = do
  varKVs <- traverse encodeDecl bindings
  let varMap = Map.fromList varKVs
  varMap' <- traverse encodeTecData varMap
  expression' <- encodeTecData expression
  return $ TecBinding varMap' expression'
encodeTecData (E.App _ lhs rhs) = do
  l <- encodeTecData lhs
  r <- encodeTecData rhs
  case l of
    (TecTypeCon typeName params) -> return $ TecTypeCon typeName (params ++ [r])
    _ -> Left $ TecError "Unexpected left side"
encodeTecData (E.Paren _ x) = encodeTecData x
encodeTecData (E.Con _ (E.UnQual _ (E.Ident _ typeName))) = Right $ TecTypeCon typeName []
encodeTecData (E.InfixApp _ left (E.QConOp _ (E.UnQual _ (E.Symbol _ op))) right) = do
  l <- encodeTecData left
  r <- encodeTecData right
  return $ TecQuery op l r
encodeTecData (E.Lit _ (E.Int _ v _)) = Right $ TecInt (fromInteger v)
encodeTecData (E.Lit _ (E.String _ v _)) = Right $ TecStr v
encodeTecData (E.EnumFrom _ e) = do
  f <- encodeTecData e
  case f of
    (TecInt i) -> Right $ TecRngInt i Nothing
    (TecTypeCon label []) -> Right $ TecRngEnum label Nothing
    _ -> Left $ TecErrorUnknownExp (show e)
encodeTecData (E.EnumFromTo l from to) = do
  f <- encodeTecData from
  t <- encodeTecData to
  case (f, t) of
    (TecInt a, TecInt b) -> Right $ TecRngInt a (Just b)
    (TecTypeCon a [], TecTypeCon b []) -> Right $ TecRngEnum a (Just b)
    _ -> Left $ TecErrorUnknownExp (show (E.EnumFromTo l from to))
encodeTecData (E.List _ exps) = traverse encodeTecData exps <&> TecList
encodeTecData e = Left $ TecErrorUnknownExp (show e)

_encodeTecType :: (Show l) => E.Decl l -> Either TecError TecTypeAST
_encodeTecType d = Left $ TecErrorUnknownExp (show d)
encodeTecType :: (Show l) => E.QualConDecl l -> Either TecError TecTypeAST
encodeTecType d = Left $ TecErrorUnknownExp (show d)
