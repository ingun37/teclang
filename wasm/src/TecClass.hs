module TecClass where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)

data TecClass = TecClass {className :: String, indexSet :: [String], attributes :: [String]} deriving (Show, Generic)

instance ToJSON TecClass where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClass

data TecClassAST = TecClassAST {classes :: [TecClass]} deriving (Show, Generic)

instance ToJSON TecClassAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClassAST
