module TecEnum where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)


data TecValue = TecValue String deriving (Show, Generic)

instance ToJSON TecValue where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecValue

data TecEnum = TecEnum {tecEnumName :: String, tecEnumValues :: [TecValue]}
  deriving (Show, Generic)

instance ToJSON TecEnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnum

data TecEnumAST = TecEnumAST {tecEnums :: [TecEnum]} deriving (Show, Generic)

instance ToJSON TecEnumAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnumAST
