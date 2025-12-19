{-# LANGUAGE TemplateHaskell #-}

module MyLib
  ( 
    TecDataAST,
    Parsed (Parsed),
    ast,
    rawAstShow,
    TecError (TecError, TecErrorUnknownExp, TecErrorWithWholeExpShow),
    tecError,
    TecAST,
    encodeCodeToTec,
    decodeTecToCode,
    TecTypeAST
  )
where

import TecTypes
import TecAST