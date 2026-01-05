module TecNode where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)
import Optics.Core

newtype TecNodeIndex = TecNodeIndex String deriving (Show, Generic)

instance ToJSON TecNodeIndex where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNodeIndex

data TecNode = TecNode
  { indexCombination :: [TecNodeIndex],
    tecNodeAttributes :: [String]
  }
  deriving (Show, Generic)

_indexCombination :: Lens' TecNode [TecNodeIndex]
_indexCombination = lens indexCombination (\(TecNode _ ys) xs' -> TecNode xs' ys)

_tecNodeAttributes :: Lens' TecNode [String]
_tecNodeAttributes = lens tecNodeAttributes (\(TecNode xs _) ys' -> TecNode xs ys')

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