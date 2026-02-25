{-# LANGUAGE GADTs #-}

module TecEncode
  ( encodeTecDataAST,
    encodeTecClassAST,
    encodeTecEnumAST,
    encodeTecNodeAST,
    encodeTecPnumAST,
    encodeTecQuery,
  )
where

import Data.Char (toUpper)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Language.Haskell.Exts qualified as E
import Optics.Core
import TecClass
import TecCommonEncode qualified as CE
import TecData (TecDataAST)
import TecEnum
import TecError
import TecNode
import TecPnum
import TecQuery

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
encodeTecDataAST _ = Left $ TecError "deprecated"

encodeQualConDecl :: (Show l) => E.QualConDecl l -> Either TecError String
encodeQualConDecl (E.QualConDecl _ Nothing Nothing (E.ConDecl _ ident _)) = getIdent ident
encodeQualConDecl x = Left $ TecErrorUnknownExp (show x)

safeLast :: [a] -> Maybe a
safeLast = foldl (\_ x -> Just x) Nothing

encodeTecEnum :: (Show l) => E.Decl l -> Either TecError TecEnum
encodeTecEnum (E.DataDecl _ (E.DataType _) Nothing (E.DHead _ (E.Ident _ name)) decls []) = do
  paramTypes <- traverse encodeQualConDecl decls
  return $ TecEnum name (map TecValue paramTypes)
encodeTecEnum x = Left $ TecErrorUnknownExp (show x)

encodeTecEnumAST :: (Show l) => [E.Decl l] -> Either TecError TecEnumAST
encodeTecEnumAST decls = do
  tecEnums <- traverse encodeTecEnum decls
  return $ TecEnumAST tecEnums

encodeTecAttributes :: (Show l) => E.Type l -> Either TecError TecAttributes
encodeTecAttributes (E.TyTuple _ E.Boxed tyCons) = do
  xs <- traverse getTyCon tyCons
  return $ TecAttributes xs
encodeTecAttributes tyCon = do
  x <- getTyCon tyCon
  return $ TecAttributes [x]

encodeTecSignature :: (Show l) => E.Type l -> Either TecError TecSignature
encodeTecSignature (E.TyFun _ tyCon right) = do
  (TecSignature idxs attribs) <- encodeTecSignature right
  idx <- getTyCon tyCon
  return $ TecSignature (idx : idxs) attribs
encodeTecSignature t = do
  attribs <- encodeTecAttributes t
  return $ TecSignature [] attribs

encodeTecIndexPattern :: (Show l) => E.Pat l -> TecEth TecIndexPattern
encodeTecIndexPattern (E.PApp _ unqual []) = TecIndexValue <$> getUnQual unqual
encodeTecIndexPattern (E.PWildCard _) = return TecIndexAll
encodeTecIndexPattern pat = unknownExp pat

encodeTecMatch :: (Show l) => E.Match l -> TecEth TecMatch
encodeTecMatch (E.Match _ _ apps (E.UnGuardedRhs _ (E.Con _ uq)) _) =
  let
   in do
        _tecIndexPatterns <- traverse encodeTecIndexPattern apps
        _tecEnumValue <- CE.encodeUQ uq
        return TecMatch {_tecIndexPatterns, _tecEnumValue}
encodeTecMatch e = unknownExp e

encodeTecPnumTable :: (Show l) => (E.Decl l, E.Decl l) -> TecEth TecPnumTable
encodeTecPnumTable (ts, fb) =
  case ts of
    (E.TypeSig _ _ funcSig) ->
      case fb of
        (E.FunBind _ matches) ->
          do
            _tecIndexTypes <- NE.init <$> CE.encodeTyFun funcSig
            _tecMatches <- traverse encodeTecMatch matches

            return $
              TecPnumTable
                { _tecIndexTypes,
                  _tecMatches
                }
        _ -> unknownExp fb
    _ -> unknownExp ts

encodeTecPnum :: (Show l) => (E.Decl l, Maybe (E.Decl l, E.Decl l)) -> TecEth TecPnum
encodeTecPnum (dd, ts_fb) = case dd of
  E.DataDecl _ _ Nothing (E.DHead _ enumNameIdent) enumValues _ -> do
    _tecPnumName <- getIdent enumNameIdent
    _tecEnumValues <- traverse CE.encodeQCD enumValues
    _tecPnumTable <- traverse encodeTecPnumTable ts_fb
    return $
      TecPnum
        { _tecPnumName,
          _tecEnumValues,
          _tecPnumTable
        }
  _ -> unknownExp dd

peekFuncName :: (Show l) => E.Decl l -> TecEth String
peekFuncName (E.TypeSig _ [E.Ident _ name] _) = return name
peekFuncName (E.FunBind _ ((E.Match _ (E.Ident _ name) _ _ _) : _)) = return name
peekFuncName e = unknownExp e

peekDataName :: (Show l) => E.Decl l -> TecEth String
peekDataName (E.TypeSig _ _ sig) = NE.last <$> CE.encodeTyFun sig
peekDataName (E.DataDecl _ _ _ (E.DHead _ (E.Ident _ name)) _ _) = return name
peekDataName e = unknownExp e

encodeTecPnumAST :: (Show l) => [E.Decl l] -> Either TecError TecPnumAST
encodeTecPnumAST decls =
  let isFunBind (E.FunBind {}) = True
      isFunBind _ = False
      isTypeSig (E.TypeSig {}) = True
      isTypeSig _ = False
      isDataDecl (E.DataDecl {}) = True
      isDataDecl _ = False
      hasFuncName s = fmap (s ==) . peekFuncName
      hasDataName s = fmap (s ==) . peekDataName . fst
      (dataDecls, rest) = List.partition isDataDecl decls
      (typeSigs, rest') = List.partition isTypeSig rest
      (funBinds, _) = List.partition isFunBind rest'
      matchFunc ts = do
        n <- peekFuncName ts
        fb <- findMOf folded (hasFuncName n) funBinds
        fb' <- maybe (tecErr $ "Failed to find FuncBind of " ++ n) return fb
        return (ts, fb')
   in do
        funcs <- traverse matchFunc typeSigs
        let matchData dd = do
              n <- peekDataName dd
              fun <- findMOf folded (hasDataName n) funcs
              return (dd, fun)
        pairs <- traverse matchData dataDecls
        tecPnums <- traverse encodeTecPnum pairs
        return $ TecPnumAST {tecPnums}

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

unfoldrM :: (Monad m) => (a -> m (Maybe (a, b))) -> a -> m [b]
unfoldrM f seed = do
  res <- f seed
  case res of
    Nothing -> return []
    Just (next, val) -> do
      rest <- unfoldrM f next
      return (val : rest)

encodeTecNodeAttributes :: (Show l) => E.Rhs l -> Either TecError [TecNodeAttribute]
encodeTecNodeAttributes (E.UnGuardedRhs _ (E.Tuple _ E.Boxed attribs)) = traverse encodeTecNodeAttribute attribs
encodeTecNodeAttributes (E.UnGuardedRhs _ con) = do
  a <- encodeTecNodeAttribute con
  return [a]
encodeTecNodeAttributes rhs = Left $ TecErrorUnknownExp (show rhs)

encodeTecNodeIndex :: (Show l) => E.Pat l -> Either TecError TecNodeIndex
encodeTecNodeIndex (E.PApp _ unqual []) = TecNodeIndexInst <$> getUnQual unqual
encodeTecNodeIndex (E.PWildCard _) = return TecNodeIndexWildcard
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

encodeTecIndexValue :: (Show l) => E.Exp l -> Either TecError String
-- encodeTecIndexValue (E.Lit _ (E.Int _ v _)) = return $ TecIndexInt v
encodeTecIndexValue (E.Con _ uq) = getUnQual uq
encodeTecIndexValue e = Left $ TecErrorUnknownExp (show e)

encodeTecIndexSeq :: (Show l) => E.Exp l -> Either TecError TecIndexSeq
encodeTecIndexSeq (E.EnumFrom _ lit) = do
  vals <- encodeTecIndexValue lit
  return $ TecIndexRange vals Nothing
encodeTecIndexSeq (E.List _ cs) = do
  vals <- traverse encodeTecIndexValue cs
  return $ TecIndexValues vals
encodeTecIndexSeq e = Left $ TecErrorUnknownExp (show e)

encodeTecQuery :: (Show l) => E.Exp l -> Either TecError TecQuery
encodeTecQuery (E.App _ (E.Var _ uq) r) = do
  tecQueryFunc <- getUnQual uq
  tecQueryIndexSeq <- encodeTecIndexSeq r
  return $ TecQuery {tecQueryFunc, tecQueryIndexSeqs = [tecQueryIndexSeq]}
encodeTecQuery (E.App _ l r) = do
  l' <- encodeTecQuery l
  r' <- encodeTecIndexSeq r
  return $ over _tecQueryIndexSeqs (++ [r']) l'
encodeTecQuery e = Left $ TecErrorUnknownExp (show e)