module TecDecode
  ( decodeTecClassAST,
    decodeTecEnumAST,
    decodeTecDataAST,
    decodeTecNodeAST,
    decodeTecPnumAST,
    decodeTecQuery,
  )
where

import Control.Monad (foldM)
import Data.Char (toLower)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Language.Haskell.Exts qualified as E
import Optics.Core
import TecClass
import TecCommonEncode qualified as CE
import TecData (TecDataAST)
import TecEnum
  ( TecEnum (TecEnum),
    TecEnumAST (TecEnumAST),
    TecValue (..),
  )
import TecError
import TecNode
import TecPnum
import TecQuery

lowerFirst :: String -> String
lowerFirst [] = [] -- Handle empty string case
lowerFirst (x : xs) = toLower x : xs

getIdent :: String -> E.Name ()
getIdent = E.Ident ()

getUnqual :: String -> E.QName ()
getUnqual x = E.UnQual () (getIdent x)

getTyCon :: String -> E.Type ()
getTyCon name = E.TyCon () (E.UnQual () (getIdent name))

getCon :: String -> E.Exp ()
getCon name = E.Con () (E.UnQual () (getIdent name))

intE :: (Integral a, Show a) => a -> E.Exp ()
intE i = E.Lit () (E.Int () (toInteger i) (show i))

decodeNodeAttribute :: TecNodeAttribute -> E.Exp ()
decodeNodeAttribute (TecNodeTextAttribute s) = E.Lit () $ E.String () s s
decodeNodeAttribute (TecNodeIntAttribute i) =
  let wrap = if i < 0 then E.Paren () . E.NegApp () else id
      a = abs i
   in wrap (E.Lit () $ E.Int () a (show a))
decodeNodeAttribute (TecNodeFracAttribute r) =
  let wrap = if r < 0 then E.Paren () . E.NegApp () else id
      a = abs r
   in wrap (E.Lit () $ E.Frac () a (show a))
decodeNodeAttribute (TecNodeConAttribute r) = E.Con () (getUnqual r)

decodeDecl :: (String, E.Exp ()) -> Either TecError (E.Decl ())
decodeDecl (varName, varExp) =
  return $ E.PatBind () (E.PVar () (getIdent varName)) (E.UnGuardedRhs () varExp) Nothing

decodeTecDataAST :: TecDataAST -> Either TecError (E.Exp ())
decodeTecDataAST _ = Left $ TecError "deprecated"

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
decodeTecAttributes (TecAttributes [a]) = do
  return $ getTyCon a
decodeTecAttributes (TecAttributes (a : as)) = do
  let xs = map getTyCon (a : as)
  return $ E.TyTuple () E.Boxed xs

decodeTecSignature :: TecSignature -> Either TecError (E.Type ())
decodeTecSignature (TecSignature idxs attribs) = do
  atts <- decodeTecAttributes attribs
  let abb a = E.TyFun () (getTyCon a)
  return $ foldr abb atts idxs

decodeTecClass :: TecClass -> Either TecError (E.Decl ())
decodeTecClass (TecClass tecClassName sig) = do
  s <- decodeTecSignature sig
  return (E.TypeSig () [getIdent (lowerFirst tecClassName)] s)

decodeTecClassAST :: TecClassAST -> Either TecError [E.Decl ()]
decodeTecClassAST (TecClassAST tecClasses) = traverse decodeTecClass tecClasses

decodeTecNodeIndex :: TecNodeIndex -> E.Pat ()
decodeTecNodeIndex TecNodeIndexWildcard = E.PWildCard ()
decodeTecNodeIndex (TecNodeIndexInst t) = E.PApp () (getUnqual t) []

decodeTecNodeAttributes :: [TecNodeAttribute] -> E.Rhs ()
decodeTecNodeAttributes [str] = E.UnGuardedRhs () (decodeNodeAttribute str)
decodeTecNodeAttributes strs = E.UnGuardedRhs () (E.Tuple () E.Boxed (map decodeNodeAttribute strs))

decodeTecNode :: String -> TecNode -> E.Match ()
decodeTecNode className (TecNode indexCombs attribs) =
  let
   in E.Match () (getIdent (lowerFirst className)) (map decodeTecNodeIndex indexCombs) (decodeTecNodeAttributes attribs) Nothing

decodeTecNodeSet :: TecNodeSet -> E.Decl ()
decodeTecNodeSet (TecNodeSet name nodes) = E.FunBind () (map (decodeTecNode name) nodes)

decodeTecNodeAST :: TecNodeAST -> [E.Decl ()]
decodeTecNodeAST (TecNodeAST nodeSets) = map decodeTecNodeSet nodeSets

decodeTecIndexValue :: String -> E.Exp ()
-- decodeTecIndexValue (TecIndexInt v) = E.Lit () (E.Int () v (show v))
-- decodeTecIndexValue (TecIndexCon c) = getCon c
decodeTecIndexValue = getCon

decodeTecIndexSeq :: TecIndexSeq -> E.Exp ()
decodeTecIndexSeq (TecIndexRange {tecIndexFrom, tecIndexTo}) =
  case tecIndexTo of
    Nothing -> E.EnumFrom () (decodeTecIndexValue tecIndexFrom)
    Just toIndexValue -> E.EnumFromTo () (decodeTecIndexValue tecIndexFrom) (decodeTecIndexValue toIndexValue)
decodeTecIndexSeq (TecIndexValues {tecIndexValues}) =
  E.List () (map decodeTecIndexValue tecIndexValues)

decodeTecQuery :: TecQuery -> Either TecError (E.Exp ())
decodeTecQuery (TecQuery {tecQueryFunc, tecQueryIndexSeqs}) =
  let bab b a = E.App () b (decodeTecIndexSeq a)
   in return $ foldl bab (E.Var () (getUnqual tecQueryFunc)) tecQueryIndexSeqs

decodeTecIndexPattern :: TecIndexPattern -> E.Pat ()
decodeTecIndexPattern TecIndexAll = E.PWildCard ()
decodeTecIndexPattern (TecIndexValue t) = E.PApp () (CE.deUQ t) []

decodeTecMatch :: String -> TecMatch -> E.Match ()
decodeTecMatch pnumName tecMatch =
  let funcName = CE.deIdent (lowerFirst pnumName)
      returnType = E.UnGuardedRhs () (E.Con () (CE.deUQ (tecMatch ^. tecEnumValue)))
   in E.Match () funcName (map decodeTecIndexPattern (tecMatch ^. tecIndexPatterns)) returnType Nothing

decodeTecPnumTable :: String -> TecPnumTable -> [E.Decl ()]
decodeTecPnumTable pnumName pnumTable =
  let abb a = E.TyFun () (CE.deTyCon a)
      tyFun = foldr abb (CE.deTyCon pnumName) (pnumTable ^. tecIndexTypes)
      tySig = E.TypeSig () [CE.deIdent (lowerFirst pnumName)] tyFun
      fBind = E.FunBind () (map (decodeTecMatch pnumName) (pnumTable ^. tecMatches))
   in [tySig, fBind]

decodeTecPnum :: TecPnum -> [E.Decl ()]
decodeTecPnum pnum =
  let vs = pnum ^. tecEnumValues
      valueQCDs = map CE.deQCD vs
      name = pnum ^. tecPnumName
      dataDecl = E.DataDecl () (E.DataType ()) Nothing (E.DHead () (CE.deIdent name)) valueQCDs []
      tableDecls = fmap (decodeTecPnumTable name) (pnum ^. tecPnumTable)
      tableDecls' = Maybe.fromMaybe [] tableDecls
   in dataDecl : tableDecls'

decodeTecPnumAST :: TecPnumAST -> [E.Decl ()]
decodeTecPnumAST (TecPnumAST pnums) = concatMap decodeTecPnum pnums
