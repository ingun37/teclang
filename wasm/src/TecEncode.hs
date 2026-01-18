module TecEncode
  ( encodeTecDataAST,
    encodeTecClassAST,
    encodeTecEnumAST,
    encodeTecNodeAST,
  )
where

import Data.Char (toUpper)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Language.Haskell.Exts qualified as E
import Optics.Core
import TecClass
import TecData
import TecEnum
import TecError
import TecNode

upperFirst :: String -> String
upperFirst [] = [] -- Handle empty string case
upperFirst (x : xs) = toUpper x : xs

getIdent :: (Show l) => E.Name l -> Either TecError String
getIdent (E.Ident _ name) = return name
getIdent d = Left $ TecErrorUnknownExp (show d)

getUnQual :: (Show l) => E.QName l -> Either TecError String
getUnQual (E.UnQual _ (E.Ident _ name)) = return name
getUnQual x = Left $ TecErrorUnknownExp (show x)

getTyCon :: (Show l) => E.Type l -> Either TecError String
getTyCon (E.TyCon _ (E.UnQual _ (E.Ident _ name))) = return name
getTyCon x = Left $ TecErrorUnknownExp (show x)

getLiteral :: (Show l) => E.Literal l -> Either TecError TecNodeAttribute
getLiteral (E.String _ s _) = return $ TecNodeTextAttribute s
getLiteral (E.Int _ i _) = return $ TecNodeIntAttribute i
getLiteral (E.Frac _ r _) = return $ TecNodeFracAttribute r
getLiteral e = Left $ TecErrorUnknownExp (show e)

encodeTecNodeAttribute :: (Show l) => E.Exp l -> Either TecError TecNodeAttribute
encodeTecNodeAttribute (E.Lit _ l) = getLiteral l
encodeTecNodeAttribute (E.Con _ unqual) = TecNodeConAttribute <$> getUnQual unqual
encodeTecNodeAttribute (E.Paren _ p) = encodeTecNodeAttribute p
encodeTecNodeAttribute (E.NegApp _ (E.Lit _ (E.Int _ i _))) = return $ TecNodeIntAttribute (-i)
encodeTecNodeAttribute (E.NegApp _ (E.Lit _ (E.Frac _ f _))) = return $ TecNodeFracAttribute (-f)
encodeTecNodeAttribute e = Left $ TecErrorUnknownExp (show e)

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

safeLast :: [a] -> Maybe a
safeLast = foldl (\_ x -> Just x) Nothing

encodeTecEnum :: (Show l) => E.Decl l -> Either TecError TecEnum
encodeTecEnum (E.DataDecl _ (E.DataType _) Nothing (E.DHead _ (E.Ident _ name)) decls []) = do
  paramTypes <- traverse encodeQualConDecl decls
  return $ TecEnum name paramTypes
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
  return $ TecClass (upperFirst tecClassName) s
encodeTecClass decl = Left $ TecErrorUnknownExp (show decl)

encodeTecClassAST :: (Show l) => [E.Decl l] -> Either TecError TecClassAST
encodeTecClassAST decls = do
  tecClasses <- traverse encodeTecClass decls
  return $ TecClassAST tecClasses

unfoldrM :: (Monad m) => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f seed = do
  res <- f seed
  case res of
    Nothing -> return []
    Just (val, next) -> do
      rest <- unfoldrM f next
      return (val : rest)

encodeTecNodeAttributes :: (Show l) => E.Rhs l -> Either TecError [TecNodeAttribute]
encodeTecNodeAttributes (E.UnGuardedRhs _ apps) = do
  let b_ab b = case b of
        (E.Con _ (E.UnQual _ (E.Ident _ _))) -> Right Nothing
        (E.App _ l r) -> do
          a <- encodeTecNodeAttribute r
          Right $ Just (a, l)
        e -> Left $ TecErrorUnknownExp (show e)
  reverse <$> unfoldrM b_ab apps
encodeTecNodeAttributes rhs = Left $ TecErrorUnknownExp (show rhs)

encodeTecNodeIndex :: (Show l) => E.Pat l -> Either TecError String
encodeTecNodeIndex (E.PApp _ unqual []) = getUnQual unqual
encodeTecNodeIndex pat = Left $ TecErrorUnknownExp (show pat)

encodeTecNodeClassName :: (Show l) => E.Match l -> Either TecError String
encodeTecNodeClassName (E.Match _ ident _ _ _) = getIdent ident
encodeTecNodeClassName m = Left $ TecErrorFormatFail (show m)

encodeTecNode :: (Show l) => E.Match l -> Either TecError TecNode
encodeTecNode (E.Match _ _ apps rhs Nothing) = do
  indexCombs <- traverse encodeTecNodeIndex apps
  attribs <- encodeTecNodeAttributes rhs
  return $ TecNode indexCombs attribs
encodeTecNode m = Left $ TecErrorUnknownExp (show m)

encodeTecNodeSet :: (Show l) => E.Decl l -> Either TecError TecNodeSet
encodeTecNodeSet (E.FunBind _ matches') = do
  matches <- maybe (Left $ TecError "matches are empty") return (NE.nonEmpty matches')
  nodes <- traverse encodeTecNode matches
  name <- encodeTecNodeClassName (NE.head matches)
  return $ TecNodeSet (upperFirst name) (NE.toList nodes)
encodeTecNodeSet m = Left $ TecErrorUnknownExp (show m)

encodeTecNodeAST :: (Show l) => [E.Decl l] -> Either TecError TecNodeAST
encodeTecNodeAST decls = do
  nodeSets <- traverse encodeTecNodeSet decls
  return $ TecNodeAST nodeSets