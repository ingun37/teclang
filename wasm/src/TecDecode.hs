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

decode :: TecDataAST -> Either TecError (E.Exp ())
decode (TecVar varName) = return (E.Var () (E.UnQual () (E.Ident () varName)))
decode (TecBinding varMap exp) = do
  varMap' <- traverse decode varMap
  decls <- traverse decodeDecl (Map.toList varMap')
  exp' <- decode exp
  return (E.Let () (E.BDecls () decls) exp')
decode (TecTypeCon typeName params) = do
  let seed = E.Con () (E.UnQual () (E.Ident () typeName))
  foldM (\e p -> decode p <&> E.App () e) seed params
decode (TecList list) = traverse decode list <&> E.List ()
decode (TecQuery op left right) = do
  l <- decode left
  r <- decode right
  return $ E.InfixApp () l (E.QConOp () (E.UnQual () (E.Symbol () op))) r
decode (TecInt i) = return $ intE i
decode (TecStr s) = return $ E.Lit () (E.String () s s)
decode (TecRngInt from to) = case to of
  Nothing -> return $ E.EnumFrom () (intE from)
  Just to' -> return $ E.EnumFromTo () (intE from) (intE to')
decode (TecRngEnum from to) = do
  f <- decode (TecTypeCon from [])
  case to of
    Nothing -> return $ E.EnumFrom () f
    Just to' -> decode (TecTypeCon to' []) <&> E.EnumFromTo () f