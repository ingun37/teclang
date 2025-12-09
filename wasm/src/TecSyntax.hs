{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TecSyntax where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import GHC.Generics (Generic)

data Side = Front | Back | Left | Right deriving (Show, Generic, Enum)

instance ToJSON Side where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Side

data TecType
  = HStack [TecType]
  | VStack [TecType]
  | TecType :- TecType
  | TecType :> TecType
  | Logo
  | Code
  | Name
  | PageNumber
  | Colorway Int
  | Colorways [Int]
  | Fabric String
  | Pantone String
  | Text String
  | Render Int Side
  | Renders [Int] [Side]
  deriving (Show, Generic)

instance ToJSON TecType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecType
