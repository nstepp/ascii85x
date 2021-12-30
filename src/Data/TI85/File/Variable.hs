
-- | Behind "Data.TI85.Parsers" this is the next highest
-- level module. This module defines the structure of a
-- TI-85 variable file.
module Data.TI85.File.Variable (
    -- * Types
    TIVar(..),
    TIVarData(..),
    VarField(..),
    VarType(..),
    -- * Utilities
    idToType,
    typeToId,
    showType
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8,Word16)

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

newtype TIVarData = TIVarData {
    varsData :: [TIVar]
    }

-- | Scalar values can either be real or complex.
-- Likewise, vectors, lists, etc can contain values
-- of either.
data VarField = VarReal | VarComplex deriving Show

-- | Possible variable types.
-- See also `Data.TI85.Var.Variable`.
data VarType = VarUnknown
    | VarValue VarField
    | VarVector VarField
    | VarList VarField
    | VarMatrix VarField
    | VarConstant VarField
    | VarEquation
    | VarString
    | VarGDBFunc
    | VarGDBPolar
    | VarGDBParam
    | VarGDBDiff
    | VarPicture
    | VarProgram
    | VarDirectory
    | VarSettingsFunc
    | VarSettingsPolar
    | VarSettingsParam
    | VarSettingsDiff
    | VarSavedWinSize
    | VarMemory
    deriving Show

-- | Convert the variable ID word from
-- a variable file into its type.
-- From https://www.ticalc.org/pub/text/calcinfo/ti86prot.txt:
--
-- +---------+-------------------+
-- | Type ID | Description       |
-- +=========+===================+
-- |      00 | Real Number       |
-- +---------+-------------------+
-- |      01 | Complex Number    |
-- +---------+-------------------+
-- |      02 | Real Vector       |
-- +---------+-------------------+
-- |      03 | Complex Vector    |
-- +---------+-------------------+
-- |      04 | Real List         |
-- +---------+-------------------+
-- |      05 | Complex List      |
-- +---------+-------------------+
-- |      06 | Real Matrix       |
-- +---------+-------------------+
-- |      07 | Complex Matrix    |
-- +---------+-------------------+
-- |      08 | Real Constant     |
-- +---------+-------------------+
-- |      09 | Complex Constant  |
-- +---------+-------------------+
-- |      0A | Equation          |
-- +---------+-------------------+
-- |      0C | String            |
-- +---------+-------------------+
-- |      0D | Function GDB      |
-- +---------+-------------------+
-- |      0E | Polar GDB         |
-- +---------+-------------------+
-- |      0F | Parametric GDB    |
-- +---------+-------------------+
-- |      10 | Differential      |
-- |         | Equation GDB      |
-- +---------+-------------------+
-- |      11 | Picture           |
-- +---------+-------------------+
-- |      12 | Program           |
-- +---------+-------------------+
-- |      15 | Directory (only   |
-- |         | used when         |
-- |         | requesting dir)   |
-- +---------+-------------------+
-- |      17 | Function Window   |
-- |         | Settings          |
-- +---------+-------------------+
-- |      18 | Polar Window      |
-- |         | Settings          |
-- +---------+-------------------+
-- |      19 | Parametric Window |
-- |         | Settings          |
-- +---------+-------------------+
-- |      1A | Differential      |
-- |         | Equation Window   |
-- |         | Settings          |
-- +---------+-------------------+
-- |      1B | Saved Window Size |
-- |         | (ZRCL)            |
-- +---------+-------------------+
-- |      1D | Memory backup     |
-- +---------+-------------------+
-- |      1E | Unknown (only used|
-- |         | when requesting   |
-- |         | var)              |
-- +---------+-------------------+
--
idToType :: Word8 -> VarType
idToType 0x00 = VarValue VarReal
idToType 0x01 = VarValue VarComplex
idToType 0x02 = VarVector VarReal
idToType 0x03 = VarVector VarComplex
idToType 0x04 = VarList VarReal
idToType 0x05 = VarList VarComplex
idToType 0x06 = VarMatrix VarReal
idToType 0x07 = VarMatrix VarComplex
idToType 0x08 = VarConstant VarReal
idToType 0x09 = VarConstant VarComplex
idToType 0x0a = VarEquation
idToType 0x0c = VarString
idToType 0x0d = VarGDBFunc
idToType 0x0e = VarGDBPolar
idToType 0x0f = VarGDBParam
idToType 0x10 = VarGDBDiff
idToType 0x11 = VarPicture
idToType 0x12 = VarProgram
idToType 0x15 = VarDirectory
idToType 0x17 = VarSettingsFunc
idToType 0x18 = VarSettingsPolar
idToType 0x19 = VarSettingsParam
idToType 0x1a = VarSettingsDiff
idToType 0x1b = VarSavedWinSize
idToType 0x1d = VarMemory
idToType 0x1e = VarUnknown
idToType _ = VarUnknown

-- | Convert the variable type into
-- its ID. See `idToType`.
typeToId :: VarType -> Word8
typeToId (VarValue VarReal) = 0x00
typeToId (VarValue VarComplex) = 0x01
typeToId (VarVector VarReal) = 0x02
typeToId (VarVector VarComplex) = 0x03
typeToId (VarList VarReal) = 0x04
typeToId (VarList VarComplex) = 0x05
typeToId (VarMatrix VarReal) = 0x06
typeToId (VarMatrix VarComplex) = 0x07
typeToId (VarConstant VarReal) = 0x08
typeToId (VarConstant VarComplex) = 0x09
typeToId VarEquation = 0x0a
typeToId VarString = 0x0c
typeToId VarGDBFunc = 0x0d
typeToId VarGDBPolar = 0x0e
typeToId VarGDBParam = 0x0f
typeToId VarGDBDiff = 0x10
typeToId VarPicture = 0x11
typeToId VarProgram = 0x12
typeToId VarDirectory = 0x15
typeToId VarSettingsFunc = 0x17
typeToId VarSettingsPolar = 0x18
typeToId VarSettingsParam = 0x19
typeToId VarSettingsDiff = 0x1a
typeToId VarSavedWinSize = 0x1b
typeToId VarMemory = 0x1d
typeToId VarUnknown = 0x1e


-- | Convert a variable type to
-- its textual representation.
showType :: VarType -> Text
showType VarUnknown = "Unknown"
showType (VarValue VarReal) = "Real Value"
showType (VarValue VarComplex) = "Complex Value"
showType (VarVector VarReal) = "Real Vector"
showType (VarVector VarComplex) = "Complex Vector"
showType (VarList VarReal) = "Real List"
showType (VarList VarComplex) = "Complex List"
showType (VarMatrix VarReal) = "Real Matrix"
showType (VarMatrix VarComplex) = "Complex Matrix"
showType (VarConstant VarReal) = "Real Constant"
showType (VarConstant VarComplex) = "Complex Constant"
showType VarEquation = "Equation"
showType VarString = "String"
showType VarGDBFunc = "Function GDB"
showType VarGDBPolar = "Polar GDB"
showType VarGDBParam = "Parametric GDB"
showType VarGDBDiff = "Differential Equation GDB"
showType VarPicture = "Picture"
showType VarProgram = "Program"
showType VarDirectory = "Directory"
showType VarSettingsFunc = "Function Settings"
showType VarSettingsPolar = "Polar Settings"
showType VarSettingsParam = "Parametric Settings"
showType VarSettingsDiff = "Differential Equation Settings"
showType VarSavedWinSize = "Saved Window Size"
showType VarMemory = "Memory Backup"

