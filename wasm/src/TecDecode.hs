module TecDecode where

import Control.Monad (foldM)

import Data.Functor ((<&>))
import Data.Map qualified as Map

import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E

import TecTypes

intE :: (Integral a, Show a) => a -> E.Exp ()
intE i = E.Lit () (E.Int () (toInteger i) (show i))

data TecEnum = TecEnum
  {label :: String, typeName :: String, value :: Int}
  deriving (Show, Generic)


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

decodeTecType :: TecTypeAST -> Either TecError (E.Exp ())
decodeTecType = undefined