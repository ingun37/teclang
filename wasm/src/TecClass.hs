module TecClass where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)


data TecClassAST = TecClassAST {indexSet :: [String], attributes::[String]} deriving (Show, Generic)

instance ToJSON TecClassAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecClassAST
