module TecDecode where

import Control.Monad (foldM)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Language.Haskell.Exts qualified as E
import TecClass
import TecData
import TecEnum
import TecError

intE :: (Integral a, Show a) => a -> E.Exp ()
intE i = E.Lit () (E.Int () (toInteger i) (show i))

decodeDecl :: (String, E.Exp ()) -> Either TecError (E.Decl ())
decodeDecl (varName, varExp) =
  return $ E.PatBind () (E.PVar () (E.Ident () varName)) (E.UnGuardedRhs () varExp) Nothing

decodeTecDataAST :: TecDataAST -> Either TecError (E.Exp ())
decodeTecDataAST (TecVar varName) = return (E.Var () (E.UnQual () (E.Ident () varName)))
decodeTecDataAST (TecBinding varMap exp) = do
  varMap' <- traverse decodeTecDataAST varMap
  decls <- traverse decodeDecl (Map.toList varMap')
  exp' <- decodeTecDataAST exp
  return (E.Let () (E.BDecls () decls) exp')
decodeTecDataAST (TecCon typeName params) = do
  let seed = E.Con () (E.UnQual () (E.Ident () typeName))
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
decodeType name = return $ E.TyCon () (E.UnQual () (E.Ident () name))

decodeQualConDecl :: TecValue -> Either TecError (E.QualConDecl ())
decodeQualConDecl (TecValue name) = do
  types <- traverse decodeType []
  return $ E.QualConDecl () Nothing Nothing (E.ConDecl () (E.Ident () name) types)

decodeTecEnum :: TecEnum -> Either TecError (E.Decl ())
decodeTecEnum (TecEnum name classes) = do
  xs <- traverse decodeQualConDecl classes
  return $ E.DataDecl () (E.DataType ()) Nothing (E.DHead () (E.Ident () name)) xs []

decodeTecEnumAST :: TecEnumAST -> Either TecError [E.Decl ()]
decodeTecEnumAST (TecEnumAST tecEnums) = traverse decodeTecEnum tecEnums

decodeTecSignature :: TecSignature -> Either TecError (E.Type ())
decodeTecSignature (TecSignature idxs [attribs]) = do
  let abb a = E.TyFun () (E.TyCon () (E.UnQual () (E.Ident () a)))
  let b = E.TyCon () (E.UnQual () (E.Ident () attribs))
  return $ foldr abb b idxs
decodeTecSignature s = do
  Left $ TecErrorUnknownExp (show s)

decodeTecClass :: TecClass -> Either TecError (E.Decl ())
decodeTecClass (TecClass className sig) = do
  s <- decodeTecSignature sig
  return (E.TypeSig () [E.Ident () className] s)

decodeTecClassAST :: TecClassAST -> Either TecError [E.Decl ()]
decodeTecClassAST (TecClassAST classes) = traverse decodeTecClass classes