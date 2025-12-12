module TecTypes where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Map (Map)
import GHC.Generics (Generic)

data TecError
  = TecError String
  | TecErrorUnknownExp {expShow :: String}
  | TecErrorWithWholeExpShow {err :: TecError, wholeExpShow :: String}
  deriving (Show)

data TecAST
  = TecType {typeName :: String, parameters :: [TecAST]}
  | TecList {list :: [TecAST]}
  | TecQuery {op :: String, left :: TecAST, right :: TecAST}
  | TecInt {int :: Int}
  | TecStr {str :: String}
  | TecRngInt {fromI :: Int, toI :: Maybe Int}
  | TecRngEnum {fromE :: String, toE :: Maybe String}
  | TecVar {varName :: String}
  | TecBinding {varMap :: Map String TecAST, expression :: TecAST}
  deriving (Show, Generic)

instance ToJSON TecAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecAST
