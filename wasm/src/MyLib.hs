{-# LANGUAGE TemplateHaskell #-}

module MyLib
  ( parseHaskellStr,
    TecAST,
    Parsed (Parsed),
    makeHaskellCode,
    ast,
    TecError (TecError, TecErrorUnknownExp),
    tecError,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Aeson qualified as J
-- import Data.ByteString qualified as BS
-- import Data.FileEmbed qualified as Embed
import Data.Text qualified as T
-- import Data.Text.Encoding qualified as TE


import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import TecSyntax (Side)

data Index
  = IndexN {number :: Word}
  | IndexS {name :: String}
  | IndexE {name :: String, value :: Int}
  | IndexR {from :: Word, to :: Maybe Word}
  | IndexU
  deriving (Show, Generic)

instance ToJSON Index where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Index

data TecAST
  = TecType {typeName :: String, index :: Index}
  | TecLayout {typeName :: String, children :: [TecAST]}
  | TecQuery {op :: String, left :: TecAST, right :: TecAST}
  deriving (Show, Generic)

instance ToJSON TecAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecAST

data TecError
  = TecError String
  | TecErrorUnknownExp {expShow :: String, wholeExpShow :: String}
  deriving (Show)

mapWholeExpShow :: (Show l) => E.Exp l -> Either TecError a -> Either TecError a
mapWholeExpShow x e = case e of
  (Left (TecErrorUnknownExp a _)) -> Left (TecErrorUnknownExp a (show x))
  a -> a

tecError :: String -> Either TecError b
tecError str = Left $ TecError str

makeIndex :: (Show l) => E.Exp l -> Either TecError Index
makeIndex (E.Lit _ (E.Int _ val _)) = Right $ IndexN $ fromInteger val
makeIndex (E.Lit _ (E.String _ val _)) = Right $ IndexS val
makeIndex (E.EnumFrom _ (E.Lit _ (E.Int _ val _))) = Right $ IndexR (fromInteger val) Nothing
makeIndex (E.EnumFromTo _ (E.Lit _ (E.Int _ fromVal _)) (E.Lit _ (E.Int _ toVal _))) = Right $ IndexR (fromInteger fromVal) (Just $ fromInteger toVal)
makeIndex (E.Con _ (E.UnQual _ (E.Ident _ val))) = do
  let decoded = J.decodeStrictText (T.pack $ "\"" ++ val ++ "\"") :: Maybe Side
  case decoded of
    Nothing -> Left $ TecError $ "Failed to parse " ++ val ++ " to Side"
    Just side -> Right $ IndexE val (fromEnum side)
makeIndex unknownExp = Left $ TecErrorUnknownExp (show unknownExp) ""

makeTecAST :: (Show l) => E.Exp l -> Either TecError TecAST
makeTecAST (E.App _ (E.Con _ (E.UnQual _ (E.Ident _ conName))) exp) =
  let handle (E.List _ exps) = do
        children <- traverse makeTecAST exps
        Right $ TecLayout conName children
      handle indexE = TecType conName <$> makeIndex indexE
   in handle exp
makeTecAST (E.Con _ (E.UnQual _ (E.Ident _ conName))) =
  Right $
    TecType conName IndexU
makeTecAST (E.InfixApp _ l (E.QConOp _ (E.UnQual _ (E.Symbol _ op))) r) = do
  left <- makeTecAST l
  right <- makeTecAST r
  Right $ TecQuery op left right
makeTecAST unknownExp = Left $ TecErrorUnknownExp (show unknownExp) (show unknownExp)

makeExp :: TecAST -> Either TecError (E.Exp ())
makeExp tecAst =
  let conN n = E.Con () (E.UnQual () (E.Ident () n))
      appN n = E.App () (conN n)
      eval (TecType {typeName, index}) =
        let con = conN typeName
            app = appN typeName
         in case index of
              IndexU -> Right con
              IndexN {number} -> Right $ app (E.Lit () (E.Int () (toInteger number) (show number)))
              IndexS {name} -> Right $ app (E.Lit () (E.String () name name))
              IndexE {name, value = _} -> Right $ app (E.Con () (E.UnQual () (E.Ident () name)))
              IndexR {from = f, to = maybeTo} -> case maybeTo of
                Nothing -> Right $ app (E.EnumFrom () (E.Lit () (E.Int () (toInteger f) (show f))))
                Just t -> Right $ app (E.EnumFromTo () (E.Lit () (E.Int () (toInteger f) (show f))) (E.Lit () (E.Int () (toInteger t) (show t))))
      eval (TecLayout {typeName, children}) = do
        xs <- traverse makeExp children
        Right $ appN typeName (E.List () xs)
      eval (TecQuery {op, left, right}) = do
        l <- makeExp left
        r <- makeExp right
        Right $ E.InfixApp () l (E.QConOp () (E.UnQual () (E.Symbol () op))) r
   in eval tecAst

extractDocExp :: E.Module l -> E.Exp l
extractDocExp (E.Module _ _ _ _ decls) = head [exp | x@(E.PatBind _ _ ((E.UnGuardedRhs _ exp)) _) <- decls]
extractDocExp _ = undefined

-- tecCode :: BS.ByteString
-- tecCode = $(Embed.embedFile "src/TecSyntax.hs")

data Parsed = Parsed
  { ast :: TecAST
  }

parseHaskellStr :: String -> Either TecError Parsed
parseHaskellStr code =
  let indented = unlines $ map ("  " ++) $ lines code
      result = E.parseFileContents ("\ndoc = " ++ indented)
   in case result of
        E.ParseOk a ->
          let e = extractDocExp a
           in do
                ast <- mapWholeExpShow e $ makeTecAST e
                Right $ Parsed {ast = ast}
        E.ParseFailed _ str ->
          tecError str

makeHaskellCode :: TecAST -> Either TecError String
makeHaskellCode ast = fmap E.prettyPrint (makeExp ast)
