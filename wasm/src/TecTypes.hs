module TecTypes where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Map (Map)
import GHC.Generics (Generic)

data TecDataAST
  = TecTypeCon {typeName :: String, parameters :: [TecDataAST]}
  | TecList {list :: [TecDataAST]}
  | TecQuery {op :: String, left :: TecDataAST, right :: TecDataAST}
  | TecInt {int :: Int}
  | TecStr {str :: String}
  | TecRngInt {fromI :: Int, toI :: Maybe Int}
  | TecRngEnum {fromE :: String, toE :: Maybe String}
  | TecVar {varName :: String}
  | TecBinding {varMap :: Map String TecDataAST, expression :: TecDataAST}
  deriving (Show, Generic)

instance ToJSON TecDataAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecDataAST

data TecTypeAST
  = TecSum {types :: [TecTypeAST]}
  | TecCon {constructor :: String}
  deriving (Show, Generic)

instance ToJSON TecTypeAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecTypeAST

data Parsed a = Parsed
  { ast :: a,
    rawAstShow :: String
  }