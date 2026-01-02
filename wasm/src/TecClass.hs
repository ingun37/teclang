module TecClass where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)

newtype TecAttributes = TecAttributes [String]
  deriving (Show, Generic)

instance ToJSON TecAttributes where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecAttributes

data TecSignature = TecSignature
  { indexSet :: [String],
    attributes :: TecAttributes
  }
  deriving (Show, Generic)

instance ToJSON TecSignature where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecSignature

data TecClass = TecClass
  { className :: String,
    tecSignature :: TecSignature
  }
  deriving (Show, Generic)

instance ToJSON TecClass where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClass

data TecClassAST = TecClassAST {classes :: [TecClass]} deriving (Show, Generic)

instance ToJSON TecClassAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClassAST
