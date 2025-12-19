{-# LANGUAGE TemplateHaskell #-}

module MyLib
  ( parseHaskellData,
    TecDataAST,
    Parsed (Parsed),
    makeHaskellData,
    ast,
    rawAstShow,
    TecError (TecError, TecErrorUnknownExp, TecErrorWithWholeExpShow),
    tecError,
  )
where

-- import Data.ByteString qualified as BS
-- import Data.FileEmbed qualified as Embed
-- import Data.Text.Encoding qualified as TE

import Language.Haskell.Exts qualified as E
import TecTypes
import TecEncode qualified as En
import TecDecode qualified as De
mapWholeExpShow :: (Show l) => E.Exp l -> Either TecError a -> Either TecError a
mapWholeExpShow x e = case e of
  (Left err) -> Left (TecErrorWithWholeExpShow err (show x))
  a -> a

tecError :: String -> Either TecError b
tecError str = Left $ TecError str

extractDocExp :: E.Module l -> E.Exp l
extractDocExp (E.Module _ _ _ _ decls) = head [exp | x@(E.PatBind _ _ ((E.UnGuardedRhs _ exp)) _) <- decls]
extractDocExp _ = undefined

data Parsed = Parsed
  { ast :: TecDataAST,
    rawAstShow :: String
  }

parseHaskellData :: String -> Either TecError Parsed
parseHaskellData code =
  let indented = unlines $ map ("  " ++) $ lines code
      result = E.parseFileContents ("\ndoc = " ++ indented)
   in case result of
        E.ParseOk a ->
          let e = extractDocExp a
           in do
                ast <- mapWholeExpShow e $ En.encodeTecData e
                Right $ Parsed {ast = ast, rawAstShow = show e}
        E.ParseFailed _ str ->
          tecError $ "Initial parsing failed:\n" ++ str

makeHaskellData :: TecDataAST -> Either TecError String
makeHaskellData ast = fmap E.prettyPrint (De.decodeTecData ast)
