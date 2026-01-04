{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TecClass where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)

newtype TecAttributes = TecAttributes [String]
  deriving (Show, Generic, Semigroup)

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
  { tecClassName :: String,
    tecSignature :: TecSignature
  }
  deriving (Show, Generic)

instance ToJSON TecClass where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClass

data TecClassAST = TecClassAST {tecClasses :: [TecClass]} deriving (Show, Generic)

instance ToJSON TecClassAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClassAST
