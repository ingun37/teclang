module TecError where

data TecError
  = TecError String
  | TecErrorUnknownExp {expShow :: String}
  | TecErrorUnknownExpWithMessage {expShow :: String, msg :: String}  
  | TecErrorWithWholeExpShow {err :: TecError, wholeExpShow :: String}
  | TecErrorFormatFail String
  deriving (Show)