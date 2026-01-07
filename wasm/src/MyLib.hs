{-# LANGUAGE TemplateHaskell #-}

module MyLib
  ( TecDataAST,
    TecError
      ( TecError,
        TecErrorUnknownExp,
        TecErrorWithWholeExpShow,
        TecErrorFormatFail
      ),
    tecError,
    TecAST,
    encodeCodeToTec,
    decodeTecToCode,
    TecEnumAST,
    formatHaskell,
    TecClassAST,
    TecNodeAST
  )
where

import TecAST
import TecClass
import TecData
import TecEnum
import TecError
import TecFormat
import TecNode