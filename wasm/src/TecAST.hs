module TecAST where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import TecTypes
import TecDecode
import TecEncode
class (Show a, Generic a, ToJSON a, FromJSON a) => TecAST a where
  decodeTecToExp :: a -> Either TecError (E.Exp ())
  encodeExpToTec :: (Show l) => E.Exp l -> Either TecError a

instance TecAST TecDataAST where
    decodeTecToExp = decodeTecData
    encodeExpToTec = encodeTecData

instance TecAST TecTypeAST where
    decodeTecToExp = decodeTecType
    encodeExpToTec = encodeTecType