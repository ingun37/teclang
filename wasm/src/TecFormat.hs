module TecFormat where

import Data.Map qualified as Map
import Data.Text qualified as T
import Ormolu.Config qualified as OC
import Ormolu.Fixity qualified as OF
import Ormolu.Parser qualified as OPa
import Ormolu.Printer qualified as OPr

import Control.Monad.Except qualified as Ex
import TecError
mapL :: (a->c) -> Either a b -> Either c b
mapL f = either (Left . f) Right

formatHaskell :: String -> Ex.ExceptT TecError IO String
formatHaskell code =
  let fixity = OF.PackageFixityMap Map.empty
      codeT = T.pack code
      numberOfLines = length $ T.lines codeT
      config = OC.regionIndicesToDeltas numberOfLines <$> OC.defaultConfig
   in do
        (_, snippets') <- OPa.parseModule config fixity "" codeT
        snippets <- Ex.liftEither $ mapL (TecErrorFormatFail . show) snippets'
        let result = OPr.printSnippets False snippets
        return $ T.unpack result
