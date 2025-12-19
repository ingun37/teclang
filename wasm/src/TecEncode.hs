module TecEncode where

import Data.Functor ((<&>))
import Language.Haskell.Exts qualified as E
import TecTypes
import Data.Map qualified as Map
encodeDecl :: (Show l) => E.Decl l -> Either TecError (String, E.Exp l)
encodeDecl (E.PatBind _ (E.PVar _ (E.Ident _ name)) (E.UnGuardedRhs _ expr) _) = Right $ (name, expr)
encodeDecl x = Left $ TecErrorUnknownExp (show x)

encode :: (Show l) => E.Exp l -> Either TecError TecDataAST
encode (E.Var _ (E.UnQual _ (E.Ident _ name))) = return $ TecVar name
encode (E.Let _ (E.BDecls _ bindings) expression) = do
  varKVs <- traverse encodeDecl bindings
  let varMap = Map.fromList varKVs
  varMap' <- traverse encode varMap
  expression' <- encode expression
  return $ TecBinding varMap' expression'
encode (E.App _ lhs rhs) = do
  l <- encode lhs
  r <- encode rhs
  case l of
    (TecType typeName params) -> return $ TecType typeName (params ++ [r])
    _ -> Left $ TecError "Unexpected left side"
encode (E.Paren _ x) = encode x
encode (E.Con _ (E.UnQual _ (E.Ident _ typeName))) = Right $ TecType typeName []
encode (E.InfixApp _ left (E.QConOp _ (E.UnQual _ (E.Symbol _ op))) right) = do
  l <- encode left
  r <- encode right
  return $ TecQuery op l r
encode (E.Lit _ (E.Int _ v _)) = Right $ TecInt (fromInteger v)
encode (E.Lit _ (E.String _ v _)) = Right $ TecStr v
encode (E.EnumFrom _ e) = do
  f <- encode e
  case f of
    (TecInt i) -> Right $ TecRngInt i Nothing
    (TecType label []) -> Right $ TecRngEnum label Nothing
    _ -> Left $ TecErrorUnknownExp (show e)
encode (E.EnumFromTo l from to) = do
  f <- encode from
  t <- encode to
  case (f, t) of
    (TecInt a, TecInt b) -> Right $ TecRngInt a (Just b)
    (TecType a [], TecType b []) -> Right $ TecRngEnum a (Just b)
    _ -> Left $ TecErrorUnknownExp (show (E.EnumFromTo l from to))
encode (E.List _ exps) = traverse encode exps <&> TecList
encode e = Left $ TecErrorUnknownExp (show e)