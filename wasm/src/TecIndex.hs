module TecIndex where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Aeson qualified as J
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import TecSyntax (Side)

data TecError
  = TecError String
  | TecErrorUnknownExp {expShow :: String, wholeExpShow :: String}
  deriving (Show)

data TecEnum = TecEnum {label :: String, value :: Int} deriving (Show, Generic)

instance ToJSON TecEnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnum

withoutLabel :: Integer -> TecEnum
withoutLabel i = TecEnum "" (fromInteger i)

data Index
  = IndexS {name :: String}
  | IndexE TecEnum
  | IndexR {from :: TecEnum, to :: Maybe TecEnum}
  | IndexU
  deriving (Show, Generic)

instance ToJSON Index where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Index

guessEnum :: String -> Either TecError TecEnum
guessEnum label =
  let decoded = J.decodeStrictText (T.pack $ "\"" ++ label ++ "\"") :: Maybe Side
   in case decoded of
        Nothing -> Left $ TecError $ "Failed to parse " ++ label ++ " to Side"
        Just side -> Right $ TecEnum label (fromEnum side)

makeEnumExp :: TecEnum -> Either TecError (E.Exp ())
makeEnumExp (TecEnum label value) =
  case label of
    "" -> Right $ E.Lit () (E.Int () (toInteger value) (show value))
    l -> Right $ E.Con () (E.UnQual () (E.Ident () l))

makeEnum :: (Show l) => E.Exp l -> Either TecError TecEnum
makeEnum (E.Lit _ (E.Int _ valueInt valueStr)) = undefined
makeEnum (E.Con _ (E.UnQual _ (E.Ident _ label))) = undefined

makeIndex :: (Show l) => E.Exp l -> Either TecError Index
makeIndex (E.Lit _ (E.Int _ val _)) = Right $ IndexE $ withoutLabel val
makeIndex (E.Lit _ (E.String _ val _)) = Right $ IndexS val
makeIndex (E.EnumFrom _ (E.Lit _ (E.Int _ val _))) =
  Right $
    IndexR
      { from = withoutLabel val,
        to = Nothing
      }
makeIndex (E.EnumFromTo _ (E.Lit _ (E.Int _ fromVal _)) (E.Lit _ (E.Int _ toVal _))) =
  Right $
    IndexR
      { from = withoutLabel fromVal,
        to = Just $ withoutLabel toVal
      }
makeIndex (E.Con _ (E.UnQual _ (E.Ident _ val))) = do
  e <- guessEnum val
  return $ IndexE e
makeIndex unknownExp = Left $ TecErrorUnknownExp (show unknownExp) ""

makeIndexExp :: Index -> Either TecError (E.Exp ())
makeIndexExp IndexU = Left $ TecError "indexu"
makeIndexExp (IndexS name) = Right $ E.Lit () (E.String () name name)
makeIndexExp (IndexE (TecEnum label value)) = case label of
  "" -> Right $ E.Lit () (E.Int () (toInteger value) (show value))
  lbl -> Right $ E.Con () (E.UnQual () (E.Ident () lbl))
makeIndexExp (IndexR from to) = do
    f <- makeEnumExp from
    case to of
        Nothing -> Right $ E.EnumFrom () f
        Just _to -> do
            t <- makeEnumExp _to
            Right $ E.EnumFromTo () f t