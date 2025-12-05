module TecSyntax where

data TecType
  = HStack [TecType]
  | VStack [TecType]
  | TecType :- TecType
  | Logo
  | Code
  | Colorway Word
  | Fabric String
  | Pantone String