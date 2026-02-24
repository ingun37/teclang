module TecQuery where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)
import Optics.Core

data TecIndexSeq
  = TecIndexValues {tecIndexValues :: [String]}
  | TecIndexRange {tecIndexFrom :: String, tecIndexTo :: Maybe String}
  deriving (Show, Generic)

instance ToJSON TecIndexSeq where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecIndexSeq

data TecQuery = TecQuery
  { tecQueryFunc :: String,
    tecQueryIndexSeqs :: [TecIndexSeq]
  }
  deriving (Show, Generic)

instance ToJSON TecQuery where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecQuery

_tecQueryIndexSeqs :: Lens' TecQuery [TecIndexSeq]
_tecQueryIndexSeqs = lens tecQueryIndexSeqs (\(TecQuery {tecQueryFunc}) tecQueryIndexSeqs -> TecQuery {tecQueryFunc, tecQueryIndexSeqs})
