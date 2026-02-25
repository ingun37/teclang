module TecCommonEncode where

import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Language.Haskell.Exts qualified as E
import TecError

encodeIdent :: (Show l) => E.Name l -> TecEth String
encodeIdent (E.Ident _ name) = return name
encodeIdent d = unknownExp d

encodeUQ :: (Show l) => E.QName l -> TecEth String
encodeUQ (E.UnQual _ i) = encodeIdent i
encodeUQ x = unknownExp x

encodeTyCon :: (Show l) => E.Type l -> TecEth String
encodeTyCon (E.TyCon _ uq) = encodeUQ uq
encodeTyCon x = unknownExp x

encodeTyFun :: (Show l) => E.Type l -> TecEth (NE.NonEmpty String)
encodeTyFun (E.TyFun _ tyCon right) = do
  cs <- encodeTyFun right
  c <- encodeTyCon tyCon
  return (c <| cs)
encodeTyFun t = do
  c <- encodeTyCon t
  return $ NE.singleton c

encodeQCD :: (Show l) => E.QualConDecl l -> TecEth String
encodeQCD (E.QualConDecl _ Nothing Nothing (E.ConDecl _ ident _)) = encodeIdent ident
encodeQCD x = unknownExp x