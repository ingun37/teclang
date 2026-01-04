module TecEnum where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)

data TecEnumRepresentationAttribute = TecEnumRepresentationAttribute {repAttribKey :: String, repAttribType :: String} deriving (Show, Generic)

instance ToJSON TecEnumRepresentationAttribute where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnumRepresentationAttribute

data TecEnumRepresentation = TecEnumRepresentation {repAttribs :: [TecEnumRepresentationAttribute]} deriving (Show, Generic)

instance ToJSON TecEnumRepresentation where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnumRepresentation

data TecValue = TecValue String deriving (Show, Generic)

instance ToJSON TecValue where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecValue

data TecEnum = TecEnum {tecEnumName :: String, tecEnumValues :: [TecValue], tecEnumRepresentation :: TecEnumRepresentation}
  deriving (Show, Generic)

instance ToJSON TecEnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnum

data TecEnumAST = TecEnumAST {tecEnums :: [TecEnum]} deriving (Show, Generic)

instance ToJSON TecEnumAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnumAST
