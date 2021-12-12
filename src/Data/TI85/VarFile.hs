{-# LANGUAGE OverloadedStrings #-}

-- | Behind "Data.TI85.Parsers" this is the next highest
-- level module. This module defines the structure of a
-- TI-85 variable file.
module Data.TI85.VarFile (
    -- * Types
    TIHeader(..),
    TIVarFile(..),
    TIVar(..),
    VarType(..),
    -- * Utilities
    idToType,
    showType
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8,Word16)

-- | The TI-85 header is common between backup files
-- and variable files.
--
-- +--------+---+---------------------------------+
-- | 8 Byte | 3 | 42 Byte                         |
-- +========+===+=================================+
-- |**TI85**|xyz| Comment                         |
-- +--------+---+---------------------------------+
--
-- where @xyz@ is always @0x1a,0x0c,0x00@.
data TIHeader = TIHeader {
    hdrSig :: ByteString, -- 8 bytes
    hdrSig2 :: ByteString, -- 3 bytes
    hdrComment :: ByteString, -- 42 bytes
    hdrDataLen :: Word16
    } deriving Show

-- | The top-level structure of a variable
-- file, containing a header, variable list,
-- and a checksum.
data TIVarFile = TIVarFile {
    header :: TIHeader,
    varsData :: [TIVar],
    checksum :: Word16
    } deriving Show

-- | The structure of a single variable.
-- For the meaning of variable IDs, see `idToType`.
data TIVar = TIVar {
    varOffset :: Word16,
    varLen :: Word16,
    varId :: Word8,
    varNameLen :: Word8,
    varName :: ByteString,
    varDataLen :: Word16,
    varData :: ByteString
    } deriving Show

-- | Possible variable types.
-- See also `Data.TI85.Var.Variable`.
data VarType = VarUnknown
    | VarRealValue
    | VarComplexValue
    | VarVector
    | VarList
    | VarMatrix
    | VarConstant
    | VarEquation
    | VarString
    | VarProgram
    deriving Show

-- | Convert the variable ID word from
-- a variable file into its type.
idToType :: Word8 -> VarType
idToType 0 = VarRealValue
idToType 1 = VarComplexValue
idToType 2 = VarVector
idToType 3 = VarVector
idToType 4 = VarList
idToType 5 = VarList
idToType 6 = VarMatrix
idToType 7 = VarMatrix
idToType 8 = VarConstant
idToType 9 = VarConstant
idToType 10 = VarEquation
idToType 12 = VarString
idToType 18 = VarProgram
idToType _ = VarUnknown

-- | Convert a variable type to
-- its textual representation.
showType :: VarType -> Text
showType VarUnknown = "Unknown"
showType VarRealValue = "Real Value"
showType VarComplexValue = "Complex Value"
showType VarVector = "Vector"
showType VarList = "List"
showType VarMatrix = "Matrix"
showType VarConstant = "Constant"
showType VarEquation = "Equation"
showType VarString = "String"
showType VarProgram = "Program"

