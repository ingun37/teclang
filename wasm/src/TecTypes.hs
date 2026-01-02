module TecTypes where

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

data TecSum = TecSum {tecTypeName :: String, classes :: [TecValue]}
  deriving (Show, Generic)

instance ToJSON TecSum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecSum

data TecTypeAST = TecTypeAST {sumTypes :: [TecSum]} deriving (Show, Generic)

instance ToJSON TecTypeAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecTypeAST
