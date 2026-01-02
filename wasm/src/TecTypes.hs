module TecTypes where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)

data TecClass = TecClass {indexSet :: [String], attributes :: [String]} deriving (Show, Generic)

instance ToJSON TecClass where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClass

data TecValue = TecValue String deriving (Show, Generic)

instance ToJSON TecValue where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecValue

data TecEnum = TecEnum {tecEnumName :: String, tecEnumValues :: [TecValue]}
  deriving (Show, Generic)

instance ToJSON TecEnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecEnum

data TecTypeAST = TecTypeAST {tecEnums :: [TecEnum], tecClasses :: [TecClass]} deriving (Show, Generic)

instance ToJSON TecTypeAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecTypeAST
