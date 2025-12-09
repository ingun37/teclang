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
import TecSyntax (Side)
import Language.Haskell.Exts qualified as E

data TecError
  = TecError String
  | TecErrorUnknownExp {expShow :: String, wholeExpShow :: String}
  deriving (Show)

data TecEnum = TecEnum {label :: String, value :: Int} deriving (Show, Generic)

instance ToJSON TecEnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnum

withoutLabel :: Int -> TecEnum
withoutLabel = TecEnum ""

data Index
  = IndexN {number :: Word}
  | IndexS {name :: String}
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

makeIndex :: (Show l) => E.Exp l -> Either TecError Index
makeIndex (E.Lit _ (E.Int _ val _)) = Right $ IndexN $ fromInteger val
makeIndex (E.Lit _ (E.String _ val _)) = Right $ IndexS val
makeIndex (E.EnumFrom _ (E.Lit _ (E.Int _ val _))) =
  Right $
    IndexR
      { from = withoutLabel (fromInteger val),
        to = Nothing
      }
makeIndex (E.EnumFromTo _ (E.Lit _ (E.Int _ fromVal _)) (E.Lit _ (E.Int _ toVal _))) =
  Right $
    IndexR
      { from = withoutLabel (fromInteger fromVal),
        to = Just $ withoutLabel (fromInteger toVal)
      }
makeIndex (E.Con _ (E.UnQual _ (E.Ident _ val))) = do
  e <- guessEnum val
  return $ IndexE e
makeIndex unknownExp = Left $ TecErrorUnknownExp (show unknownExp) ""
