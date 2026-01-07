module TecDecode
  ( decodeTecClassAST,
    decodeTecEnumAST,
    decodeTecDataAST,
    decodeTecNodeAST,
  )
where

import Control.Monad (foldM)
import Data.Functor ((<&>))
import Data.List (unsnoc)
import Data.Map qualified as Map
import Language.Haskell.Exts qualified as E
import Optics.Core
import TecClass
import TecData
import TecEnum
import TecError
import TecNode

getIdent :: String -> E.Name ()
getIdent = E.Ident ()

getTyCon :: String -> E.Type ()
getTyCon name = E.TyCon () (E.UnQual () (getIdent name))

getCon :: String -> E.Exp ()
getCon name = E.Con () (E.UnQual () (getIdent name))

getPVar :: String -> E.Pat ()
getPVar s = E.PVar () (getIdent s)

intE :: (Integral a, Show a) => a -> E.Exp ()
intE i = E.Lit () (E.Int () (toInteger i) (show i))

decodeDecl :: (String, E.Exp ()) -> Either TecError (E.Decl ())
decodeDecl (varName, varExp) =
  return $ E.PatBind () (E.PVar () (getIdent varName)) (E.UnGuardedRhs () varExp) Nothing

decodeTecDataAST :: TecDataAST -> Either TecError (E.Exp ())
decodeTecDataAST (TecVar varName) = return (E.Var () (E.UnQual () (getIdent varName)))
decodeTecDataAST (TecBinding varMap exp) = do
  varMap' <- traverse decodeTecDataAST varMap
  decls <- traverse decodeDecl (Map.toList varMap')
  exp' <- decodeTecDataAST exp
  return (E.Let () (E.BDecls () decls) exp')
decodeTecDataAST (TecCon typeName params) = do
  let seed = E.Con () (E.UnQual () (getIdent typeName))
  foldM (\e p -> decodeTecDataAST p <&> E.App () e) seed params
decodeTecDataAST (TecList list) = traverse decodeTecDataAST list <&> E.List ()
decodeTecDataAST (TecQuery op left right) = do
  l <- decodeTecDataAST left
  r <- decodeTecDataAST right
  return $ E.InfixApp () l (E.QConOp () (E.UnQual () (E.Symbol () op))) r
decodeTecDataAST (TecInt i) = return $ intE i
decodeTecDataAST (TecStr s) = return $ E.Lit () (E.String () s s)
decodeTecDataAST (TecRngInt from to) = case to of
  Nothing -> return $ E.EnumFrom () (intE from)
  Just to' -> return $ E.EnumFromTo () (intE from) (intE to')
decodeTecDataAST (TecRngEnum from to) = do
  f <- decodeTecDataAST (TecCon from [])
  case to of
    Nothing -> return $ E.EnumFrom () f
    Just to' -> decodeTecDataAST (TecCon to' []) <&> E.EnumFromTo () f

decodeType :: String -> Either TecError (E.Type ())
decodeType name = return $ getTyCon name

decodeQualConDecl :: TecValue -> Either TecError (E.QualConDecl ())
decodeQualConDecl (TecValue name) = do
  types <- traverse decodeType []
  return $ E.QualConDecl () Nothing Nothing (E.ConDecl () (getIdent name) types)

decodeTecEnum :: TecEnum -> Either TecError (E.Decl ())
decodeTecEnum (TecEnum name classes) = do
  xs <- traverse decodeQualConDecl classes
  return $ E.DataDecl () (E.DataType ()) Nothing (E.DHead () (getIdent name)) xs []

decodeTecEnumAST :: TecEnumAST -> Either TecError [E.Decl ()]
decodeTecEnumAST (TecEnumAST tecEnums) = traverse decodeTecEnum tecEnums

decodeTecAttributes :: TecAttributes -> Either TecError (E.Type ())
decodeTecAttributes (TecAttributes []) = do
  Left $ TecError "TecAttributes are empty"
decodeTecAttributes (TecAttributes (attrib : attribs)) = do
  let bab b a = E.TyApp () b (getTyCon a)
  return $ foldl bab (getTyCon attrib) attribs

decodeTecSignature :: TecSignature -> Either TecError (E.Type ())
decodeTecSignature (TecSignature idxs attribs) = do
  atts <- decodeTecAttributes attribs
  let abb a = E.TyFun () (getTyCon a)
  return $ foldr abb atts idxs

decodeTecClass :: TecClass -> Either TecError (E.Decl ())
decodeTecClass (TecClass tecClassName sig) = do
  s <- decodeTecSignature sig
  return (E.TypeSig () [getIdent tecClassName] s)

decodeTecClassAST :: TecClassAST -> Either TecError [E.Decl ()]
decodeTecClassAST (TecClassAST tecClasses) = traverse decodeTecClass tecClasses

trivialOp :: E.QOp ()
trivialOp = E.QConOp () (E.Special () (E.Cons ()))

decodeTecNode :: TecNode -> Either TecError (E.Exp ())
decodeTecNode (TecNode idxs (att : atts)) = do
  let bab b a = E.App () b (getCon a)
  let attribs = foldl bab (getCon att) atts
  case Data.List.unsnoc idxs of
    Nothing -> return attribs
    Just (is, i) -> do
      let abb a = E.InfixApp () (getCon (view _tecNodeIndex a)) trivialOp
      let indexs = foldr abb (getCon (view _tecNodeIndex i)) is
      return $ E.InfixApp () indexs trivialOp attribs
decodeTecNode s = Left $ TecErrorUnknownExp (show s)

decodeTecNodeSet :: TecNodeSet -> Either TecError (E.Decl ())
decodeTecNodeSet (TecNodeSet tecNodeClass tecNodes) = do
  nodes <- traverse decodeTecNode tecNodes
  return $ E.PatBind () (getPVar tecNodeClass) (E.UnGuardedRhs () (E.List () nodes)) Nothing

decodeTecNodeAST :: TecNodeAST -> Either TecError [E.Decl ()]
decodeTecNodeAST t = traverse decodeTecNodeSet (tecNodeSets t)