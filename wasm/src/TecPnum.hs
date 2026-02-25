{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{- HLINT ignore "Use newtype instead of data" -}

module TecPnum where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)
import Optics.TH

data TecIndexPattern = TecIndexValue String | TecIndexAll deriving (Show, Generic)

instance ToJSON TecIndexPattern where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecIndexPattern

data TecMatch = TecMatch
  { _tecIndexPatterns :: [TecIndexPattern],
    _tecEnumValue :: String
  }
  deriving (Show, Generic)

instance ToJSON TecMatch where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecMatch

data TecPnum = TecPnum
  { _tecPnumName :: String,
    _tecEnumValues :: [String],
    _tecIndexTypes :: [String],
    _tecMatches :: [TecMatch]
  }
  deriving (Show, Generic)

makeLenses ''TecPnum

instance ToJSON TecPnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecPnum

data TecPnumAST = TecPnumAST {tecPnums :: [TecPnum]} deriving (Show, Generic)

instance ToJSON TecPnumAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecPnumAST
