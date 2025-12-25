{-# LANGUAGE TemplateHaskell #-}

module MyLib
  ( TecDataAST,
    Parsed (Parsed),
    ast,
    rawAstShow,
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
    TecTypeAST,
    formatHaskell,
  )
where

import TecAST
import TecError
import TecFormat
import TecTypes
