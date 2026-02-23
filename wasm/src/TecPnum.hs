{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TecPnum where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)
import Optics.Core

data TecPnum = TecPnum
  { pnumName :: String,
    indexTypeSet :: [String]
  }
  deriving (Show, Generic)

_indexTypeSet :: Lens' TecPnum [String]
_indexTypeSet = lens indexTypeSet (\(TecPnum x _) xs' -> TecPnum x xs')


instance ToJSON TecPnum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecPnum

data TecPnumAST = TecPnumAST {tecPnums :: [TecPnum]} deriving (Show, Generic)

instance ToJSON TecPnumAST where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecPnumAST
