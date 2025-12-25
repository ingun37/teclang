module TecError where

data TecError
  = TecError String
  | TecErrorUnknownExp {expShow :: String}
  | TecErrorWithWholeExpShow {err :: TecError, wholeExpShow :: String}
  | TecErrorFormatFail String
  deriving (Show)