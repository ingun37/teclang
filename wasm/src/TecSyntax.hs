{-# LANGUAGE DeriveGeneric #-}

module TecSyntax where
import GHC.Generics ( Generic )
import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
data Side = Front | Back | Left | Right deriving(Show, Generic, Enum)
instance ToJSON Side where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Side

data TecType
  = HStack [TecType]
  | VStack [TecType]
  | TecType :- TecType
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
