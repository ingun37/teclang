module TecData where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)
import Data.Map (Map)

data TecDataAST
  = TecCon {className :: String, parameters :: [TecDataAST]}
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
