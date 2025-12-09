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

-- import Data.ByteString qualified as BS
-- import Data.FileEmbed qualified as Embed
-- import Data.Text.Encoding qualified as TE

import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import TecIndex

data TecAST
  = TecType {typeName :: String, index :: Index, index1 :: Maybe Index}
  | TecLayout {typeName :: String, children :: [TecAST]}
  | TecQuery {op :: String, left :: TecAST, right :: TecAST}
  deriving (Show, Generic)

instance ToJSON TecAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecAST

mapWholeExpShow :: (Show l) => E.Exp l -> Either TecError a -> Either TecError a
mapWholeExpShow x e = case e of
  (Left (TecErrorUnknownExp a _)) -> Left (TecErrorUnknownExp a (show x))
  a -> a

tecError :: String -> Either TecError b
tecError str = Left $ TecError str



makeTecAST :: (Show l) => E.Exp l -> Either TecError TecAST
makeTecAST (E.App _ (E.Con _ (E.UnQual _ (E.Ident _ conName))) rhs) =
  let handle (E.List _ exps) = do
        children <- traverse makeTecAST exps
        Right $ TecLayout conName children
      handle indexE = do
        i <- makeIndex indexE
        return $ TecType conName i Nothing
   in handle rhs
makeTecAST (E.App _ lhs rhs) = do
  l <- makeTecAST lhs
  r <- makeIndex rhs
  case l of
    (TecType typeName idx0 Nothing) -> return $ TecType typeName idx0 (Just r)
    _ -> Left $ TecError "Unexpected left side"
makeTecAST (E.Con _ (E.UnQual _ (E.Ident _ conName))) =
  Right $
    TecType conName IndexU Nothing
makeTecAST (E.InfixApp _ l (E.QConOp _ (E.UnQual _ (E.Symbol _ op))) r) = do
  left <- makeTecAST l
  right <- makeTecAST r
  Right $ TecQuery op left right
makeTecAST unknownExp = Left $ TecErrorUnknownExp (show unknownExp) (show unknownExp)



makeExp :: TecAST -> Either TecError (E.Exp ())
makeExp tecAst =
  let conN n = E.Con () (E.UnQual () (E.Ident () n))
      appN n = E.App () (conN n)
      eval (TecType {typeName, index, index1}) =
        let con = conN typeName
            app = appN typeName
         in case index of
              IndexU -> Right con
              i0 -> do
                iexp0 <- makeIndexExp i0
                case index1 of
                  Nothing -> Right $ app iexp0
                  Just idx1 -> do
                    iexp1 <- makeIndexExp idx1
                    Right $ E.App () (E.App () con iexp0) iexp1
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
