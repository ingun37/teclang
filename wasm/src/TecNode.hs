{-# LANGUAGE DerivingVia #-}

module TecNode where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)
import Optics.Core
data TecNodeIndex = TecNodeIndexInst String | TecNodeIndexWildcard deriving (Show, Generic)

instance ToJSON TecNodeIndex where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNodeIndex

data TecNodeAttribute
  = TecNodeConAttribute String
  | TecNodeTextAttribute String
  | TecNodeIntAttribute Integer
  | TecNodeFracAttribute Rational
  deriving (Show, Generic)

instance ToJSON TecNodeAttribute where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecNodeAttribute

data TecNode = TecNode
  { indexCombination :: [TecNodeIndex],
    tecNodeAttributes :: [TecNodeAttribute]
  }
  deriving (Show, Generic)

_indexCombination :: Lens' TecNode [TecNodeIndex]
_indexCombination = lens indexCombination (\(TecNode _ ys) xs' -> TecNode xs' ys)

_tecNodeAttributes :: Lens' TecNode [TecNodeAttribute]
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