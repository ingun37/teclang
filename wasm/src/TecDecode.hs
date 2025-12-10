module TecDecode where

import Control.Monad (foldM)
import Data.Functor ((<&>))
import Language.Haskell.Exts qualified as E
import TecTypes
import GHC.Generics (Generic)
import TecSyntax (Side)
import Data.Aeson qualified as J
import Data.Text qualified as T

intE :: (Integral a, Show a) => a -> E.Exp ()
intE i = E.Lit () (E.Int () (toInteger i) (show i))

data TecEnum = TecEnum
  {label :: String, typeName :: String, value :: Int}
  deriving (Show, Generic)

guessEnum :: String -> Either TecError TecEnum
guessEnum label =
  let decoded = J.decodeStrictText (T.pack $ "\"" ++ label ++ "\"") :: Maybe Side
   in case decoded of
        Nothing -> Left $ TecError $ "Failed to parse " ++ label ++ " to Side"
        Just side -> Right $ TecEnum label "Side" (fromEnum side)

decode :: TecAST -> Either TecError (E.Exp ())
decode (TecType typeName params) = do
  let seed = E.Con () (E.UnQual () (E.Ident () typeName))
  foldM (\e p -> decode p <&> E.App () e) seed params
decode (TecList list) = traverse decode list <&> E.List ()
decode (TecQuery op left right) = do
  l <- decode left
  r <- decode right
  return $ E.InfixApp () l (E.QConOp () (E.UnQual () (E.Symbol () op))) r


decode (TecInt i) = return $ intE i
decode (TecStr s) = return $ E.Lit () (E.String () s s)
decode (TecRngInt from to) = case to of
  Nothing -> return $ E.EnumFrom () (intE from)
  Just to' -> return $ E.EnumFromTo () (intE from) (intE to')
decode (TecRngEnum from to) = do
    f <- decode (TecType from [])
    case to of
        Nothing -> return $ E.EnumFrom () f
        Just to' -> decode (TecType to' []) <&> E.EnumFromTo () f