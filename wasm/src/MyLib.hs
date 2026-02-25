{-# LANGUAGE TemplateHaskell #-}

module MyLib
  ( TecDataAST,
    TecError (..),
    tecError,
    TecAST,
    encodeCodeToTec,
    decodeTecToCode,
    TecEnumAST,
    formatHaskell,
    TecClassAST,
    TecNodeAST,
    TecPnumAST,
    TecQuery,
  )
where

import TecAST
import TecClass
import TecData
import TecEnum
import TecError
import TecFormat
import TecNode
import TecPnum
import TecQuery