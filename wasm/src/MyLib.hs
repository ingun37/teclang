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
  )
where

import TecAST
import TecError
import TecFormat
import TecEnum
import TecData