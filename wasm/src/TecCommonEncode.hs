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

data EasyMatch l = EasyMatch
  { _easyMatchName :: String,
    _easyMatchPatterns :: [E.Pat l],
    _easyMatchRhs :: E.Rhs l
  }

encodeEasyMatch :: (Show l) => E.Match l -> TecEth (EasyMatch l)
encodeEasyMatch (E.Match _ name pats rhs Nothing) = do
  name' <- encodeIdent name
  return $ EasyMatch name' pats rhs
encodeEasyMatch e = unknownExp e

data EasyDecl l
  = EasyDataDecl
      { _easyDataDeclName :: String,
        _easyDataCons :: [String]
      }
  | EasyTypeSig
      { _easyTypeSigName :: String,
        _easyTypeSigIndexTypes :: [String],
        _easyTypeSigReturnType :: String
      }
  | EasyFunBind
      { _easyFunBindName :: String,
        _easyFunBindMatches :: NE.NonEmpty (EasyMatch l)
      }

encodeEasyDecl :: (Show l) => E.Decl l -> Either TecError (EasyDecl l)
encodeEasyDecl (E.DataDecl _ (E.DataType _) Nothing (E.DHead _ enumNameIdent) enumValues []) =
  do
    _easyDataDeclName <- encodeIdent enumNameIdent
    _easyDataCons <- traverse encodeQCD enumValues
    return $ EasyDataDecl {_easyDataDeclName, _easyDataCons}
encodeEasyDecl (E.TypeSig _ [funcName] funcSig) =
  do
    _easyTypeSigName <- encodeIdent funcName
    sigs <- encodeTyFun funcSig
    let _easyTypeSigIndexTypes = NE.init sigs
    let _easyTypeSigReturnType = NE.last sigs
    return $
      EasyTypeSig
        { _easyTypeSigName,
          _easyTypeSigIndexTypes,
          _easyTypeSigReturnType
        }
encodeEasyDecl (E.FunBind _ matches) = do
  matches' <- maybe (tecErr "matches are empty") return (NE.nonEmpty matches)
  _easyFunBindMatches <- traverse encodeEasyMatch matches'
  let _easyFunBindName = _easyMatchName (NE.head _easyFunBindMatches)
  return $ EasyFunBind {_easyFunBindName, _easyFunBindMatches}
encodeEasyDecl e = unknownExp e