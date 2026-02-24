module TecError where

data TecError
  = TecError String
  | TecErrorUnknownExp {expShow :: String}
  | TecErrorUnknownExpWithMessage {expShow :: String, msg :: String}
  | TecErrorWithWholeExpShow {err :: TecError, wholeExpShow :: String}
  | TecErrorFormatFail String
  deriving (Show)

type TecEth = Either TecError

unknownExp :: Show a => a -> Either TecError b
unknownExp e = Left $ TecErrorUnknownExp (show e)

tecErr :: String -> Either TecError b
tecErr msg = Left $ TecError msg