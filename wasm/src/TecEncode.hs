module TecEncode where

import Data.Functor ((<&>))
import Language.Haskell.Exts qualified as E
import TecTypes

encode :: (Show l) => E.Exp l -> Either TecError TecAST
encode (E.App _ lhs rhs) = do
  l <- encode lhs
  r <- encode rhs
  case l of
    (TecType typeName params) -> return $ TecType typeName (params ++ [r])
    _ -> Left $ TecError "Unexpected left side"
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