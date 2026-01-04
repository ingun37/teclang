module TecEncode (encodeTecDataAST, encodeTecClassAST, encodeTecEnumAST) where

import Data.Functor ((<&>))
import Data.Map qualified as Map
import Language.Haskell.Exts qualified as E
import TecClass
import TecData
import TecEnum
import TecError

getIdent :: (Show l) => E.Name l -> Either TecError String
getIdent (E.Ident _ name) = return name
getIdent d = Left $ TecErrorUnknownExp (show d)

getTyCon :: (Show l) => E.Type l -> Either TecError String
getTyCon (E.TyCon _ (E.UnQual _ (E.Ident _ name))) = return name
getTyCon x = Left $ TecErrorUnknownExp (show x)

encodeDecl :: (Show l) => E.Decl l -> Either TecError (String, E.Exp l)
encodeDecl (E.PatBind _ (E.PVar _ (E.Ident _ name)) (E.UnGuardedRhs _ expr) _) = Right $ (name, expr)
encodeDecl x = Left $ TecErrorUnknownExp (show x)

encodeTecDataAST :: (Show l) => E.Exp l -> Either TecError TecDataAST
encodeTecDataAST (E.Var _ (E.UnQual _ (E.Ident _ name))) = return $ TecVar name
encodeTecDataAST (E.Let _ (E.BDecls _ bindings) expression) = do
  varKVs <- traverse encodeDecl bindings
  let varMap = Map.fromList varKVs
  varMap' <- traverse encodeTecDataAST varMap
  expression' <- encodeTecDataAST expression
  return $ TecBinding varMap' expression'
encodeTecDataAST (E.App _ lhs rhs) = do
  l <- encodeTecDataAST lhs
  r <- encodeTecDataAST rhs
  case l of
    (TecCon typeName params) -> return $ TecCon typeName (params ++ [r])
    _ -> Left $ TecError "Unexpected left side"
encodeTecDataAST (E.Paren _ x) = encodeTecDataAST x
encodeTecDataAST (E.Con _ (E.UnQual _ (E.Ident _ typeName))) = Right $ TecCon typeName []
encodeTecDataAST (E.InfixApp _ left (E.QConOp _ (E.UnQual _ (E.Symbol _ op))) right) = do
  l <- encodeTecDataAST left
  r <- encodeTecDataAST right
  return $ TecQuery op l r
encodeTecDataAST (E.Lit _ (E.Int _ v _)) = Right $ TecInt (fromInteger v)
encodeTecDataAST (E.Lit _ (E.String _ v _)) = Right $ TecStr v
encodeTecDataAST (E.EnumFrom _ e) = do
  f <- encodeTecDataAST e
  case f of
    (TecInt i) -> Right $ TecRngInt i Nothing
    (TecCon label []) -> Right $ TecRngEnum label Nothing
    _ -> Left $ TecErrorUnknownExp (show e)
encodeTecDataAST (E.EnumFromTo l from to) = do
  f <- encodeTecDataAST from
  t <- encodeTecDataAST to
  case (f, t) of
    (TecInt a, TecInt b) -> Right $ TecRngInt a (Just b)
    (TecCon a [], TecCon b []) -> Right $ TecRngEnum a (Just b)
    _ -> Left $ TecErrorUnknownExp (show (E.EnumFromTo l from to))
encodeTecDataAST (E.List _ exps) = traverse encodeTecDataAST exps <&> TecList
encodeTecDataAST e = Left $ TecErrorUnknownExp (show e)

encodeQualConDecl :: (Show l) => E.QualConDecl l -> Either TecError TecValue
encodeQualConDecl (E.QualConDecl _ Nothing Nothing (E.ConDecl _ ident _)) = do
  name <- getIdent ident
  return $ TecValue name
encodeQualConDecl x = Left $ TecErrorUnknownExp (show x)

encodeTecEnumRep :: (Show l) => [E.Type l] -> Either TecError TecEnumRepresentation
encodeTecEnumRep decls = do
  attribs <- traverse getTyCon decls
  return $ TecEnumRepresentation attribs

safeLast :: [a] -> Maybe a
safeLast = foldl (\_ x -> Just x) Nothing

encodeTecEnum :: (Show l) => E.Decl l -> Either TecError TecEnum
encodeTecEnum (E.DataDecl _ (E.DataType _) Nothing (E.DHead _ (E.Ident _ name)) decls []) = do
  paramTypes <- traverse encodeQualConDecl decls
  let lastFieldDecls = safeLast [(x:xs) | E.QualConDecl _ Nothing Nothing (E.ConDecl _ _ (x:xs)) <- decls]
  rep <- maybe (Right $ TecEnumRepresentation []) encodeTecEnumRep lastFieldDecls
  return $ TecEnum name paramTypes rep
encodeTecEnum x = Left $ TecErrorUnknownExp (show x)

encodeTecEnumAST :: (Show l) => [E.Decl l] -> Either TecError TecEnumAST
encodeTecEnumAST decls = do
  tecEnums <- traverse encodeTecEnum decls
  return $ TecEnumAST tecEnums

encodeTecAttributes :: (Show l) => E.Type l -> Either TecError TecAttributes
encodeTecAttributes (E.TyApp _ left right) = do
  l <- encodeTecAttributes left
  r <- encodeTecAttributes right
  return $ l <> r
encodeTecAttributes tyCon = do
  y <- getTyCon tyCon
  return $ TecAttributes [y]

encodeTecSignature :: (Show l) => E.Type l -> Either TecError TecSignature
encodeTecSignature (E.TyFun _ tyCon right) = do
  (TecSignature idxs attribs) <- encodeTecSignature right
  idx <- getTyCon tyCon
  return $ TecSignature (idx : idxs) attribs
encodeTecSignature t = do
  attribs <- encodeTecAttributes t
  return $ TecSignature [] attribs

encodeTecClass :: (Show l) => E.Decl l -> Either TecError TecClass
encodeTecClass (E.TypeSig _ [ident] sig) = do
  s <- encodeTecSignature sig
  tecClassName <- getIdent ident
  return $ TecClass tecClassName s
encodeTecClass decl = Left $ TecErrorUnknownExp (show decl)

encodeTecClassAST :: (Show l) => [E.Decl l] -> Either TecError TecClassAST
encodeTecClassAST decls = do
  tecClasses <- traverse encodeTecClass decls
  return $ TecClassAST tecClasses