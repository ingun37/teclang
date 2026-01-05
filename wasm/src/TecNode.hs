module TecNode where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)

data TecNodeAttribute = TecTecNodeAttribute {tecAttType :: String, tecAttValue :: String} deriving (Show, Generic)

instance ToJSON TecNodeAttribute where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNodeAttribute

data TecNodeIndex = TecNodeIndex {tecIdxType :: String, tecIdxValue :: String} deriving (Show, Generic)

instance ToJSON TecNodeIndex where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNodeIndex

data TecNode = TecNode
  { indexCombination :: [TecNodeIndex],
    tecNodeAttributes :: [TecNodeAttribute]
  }
  deriving (Show, Generic)

instance ToJSON TecNode where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNode

data TecNodeSet = TecNodeSet
  { tecNodeClass :: String,
    tecNodeSet :: [TecNode]
  }
  deriving (Show, Generic)

instance ToJSON TecNodeSet where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNodeSet

newtype TecNodeAST = TecNodeAST
  { tecNodeSets :: [TecNodeSet]
  }
  deriving (Show, Generic)

instance ToJSON TecNodeAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNodeAST