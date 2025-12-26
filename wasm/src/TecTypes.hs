module TecTypes where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)


data TecTypeAST
  = TecSum {tecTypes :: [TecTypeAST]}
  | TecClass {className :: String, parameterTypes :: [String]}
  deriving (Show, Generic)

instance ToJSON TecTypeAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecTypeAST

data Parsed a = Parsed
  { ast :: a,
    rawAstShow :: String
  }