module TecDecode where

import Control.Monad (foldM)

import Data.Functor ((<&>))
import Data.Map qualified as Map

import Language.Haskell.Exts qualified as E
import TecData
import TecTypes
import TecError

intE :: (Integral a, Show a) => a -> E.Exp ()
intE i = E.Lit () (E.Int () (toInteger i) (show i))

decodeDecl :: (String, E.Exp ()) -> Either TecError (E.Decl ())
decodeDecl (varName, varExp) =
  return $ E.PatBind () (E.PVar () (E.Ident () varName)) (E.UnGuardedRhs () varExp) Nothing

decodeTecData :: TecDataAST -> Either TecError (E.Exp ())
decodeTecData (TecVar varName) = return (E.Var () (E.UnQual () (E.Ident () varName)))
decodeTecData (TecBinding varMap exp) = do
  varMap' <- traverse decodeTecData varMap
  decls <- traverse decodeDecl (Map.toList varMap')
  exp' <- decodeTecData exp
  return (E.Let () (E.BDecls () decls) exp')
decodeTecData (TecTypeCon typeName params) = do
  let seed = E.Con () (E.UnQual () (E.Ident () typeName))
  foldM (\e p -> decodeTecData p <&> E.App () e) seed params
decodeTecData (TecList list) = traverse decodeTecData list <&> E.List ()
decodeTecData (TecQuery op left right) = do
  l <- decodeTecData left
  r <- decodeTecData right
  return $ E.InfixApp () l (E.QConOp () (E.UnQual () (E.Symbol () op))) r
decodeTecData (TecInt i) = return $ intE i
decodeTecData (TecStr s) = return $ E.Lit () (E.String () s s)
decodeTecData (TecRngInt from to) = case to of
  Nothing -> return $ E.EnumFrom () (intE from)
  Just to' -> return $ E.EnumFromTo () (intE from) (intE to')
decodeTecData (TecRngEnum from to) = do
  f <- decodeTecData (TecTypeCon from [])
  case to of
    Nothing -> return $ E.EnumFrom () f
    Just to' -> decodeTecData (TecTypeCon to' []) <&> E.EnumFromTo () f

decodeType :: String -> Either TecError (E.Type ())
decodeType name = return $ E.TyCon () (E.UnQual () (E.Ident () name))

decodeTecType :: TecTypeAST -> Either TecError [E.QualConDecl ()]
decodeTecType (TecClass name params) = do
  xs <- traverse decodeType params
  return [E.QualConDecl () Nothing Nothing (E.ConDecl () (E.Ident () name) xs)]
decodeTecType (TecSum asts) = do
  decls <- traverse decodeTecType asts
  return $ concat decls
decodeTecType _ = Left $ TecError "decode fail"