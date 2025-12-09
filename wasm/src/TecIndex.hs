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

data TecEnum = TecEnum
  {label :: String, typeName :: String, value :: Int}
  deriving (Show, Generic)

instance ToJSON TecEnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnum

withoutLabel :: Integer -> TecEnum
withoutLabel i = TecEnum "" "" (fromInteger i)

data Index
  = IndexS {name :: String}
  | IndexN Int
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
        Just side -> Right $ TecEnum label "Side" (fromEnum side)

makeEnumExp :: TecEnum -> Either TecError (E.Exp ())
makeEnumExp (TecEnum label _ value) =
  case label of
    "" -> Right $ E.Lit () (E.Int () (toInteger value) (show value))
    l -> Right $ E.Con () (E.UnQual () (E.Ident () l))

makeEnum :: (Show l) => E.Exp l -> Either TecError TecEnum
makeEnum (E.Lit _ (E.Int _ valueInt _)) = Right $ withoutLabel valueInt
makeEnum (E.Con _ (E.UnQual _ (E.Ident _ label))) = do
  enum <- guessEnum label
  Right enum
makeEnum _ = Left $ TecError "Failed to make Enum from exp"

makeIndex :: (Show l) => E.Exp l -> Either TecError Index
makeIndex (E.Lit _ (E.String _ val _)) = Right $ IndexS val
makeIndex (E.EnumFrom _ e) = do
  e' <- makeEnum e
  Right $
    IndexR
      { from = e',
        to = Nothing
      }
makeIndex (E.EnumFromTo _ from to) = do
  from' <- makeEnum from
  to' <- makeEnum to
  Right $
    IndexR
      { from = from',
        to = Just to'
      }
makeIndex x = IndexE <$> makeEnum x

makeIndexExp :: Index -> Either TecError (E.Exp ())
makeIndexExp IndexU = Left $ TecError "indexu"
makeIndexExp (IndexS name) = Right $ E.Lit () (E.String () name name)
makeIndexExp (IndexE (TecEnum label _ value)) = case label of
  "" -> Right $ E.Lit () (E.Int () (toInteger value) (show value))
  lbl -> Right $ E.Con () (E.UnQual () (E.Ident () lbl))
makeIndexExp (IndexR from to) = do
  f <- makeEnumExp from
  case to of
    Nothing -> Right $ E.EnumFrom () f
    Just _to -> do
      t <- makeEnumExp _to
      Right $ E.EnumFromTo () f t
makeIndexExp _ = Left $ TecError "Failed to parse index exp"