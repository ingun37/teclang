{-# LANGUAGE DeriveGeneric #-}

module TecSyntax where
import GHC.Generics ( Generic )
import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )

data TecType
  = HStack [TecType]
  | VStack [TecType]
  | TecType :- TecType
  | Logo
  | Code
  | Name
  | PageNumber
  | Colorway Word
  | Fabric String
  | Pantone String
  | Text String
  deriving (Show, Generic)


instance ToJSON TecType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TecType
