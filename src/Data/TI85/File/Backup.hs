
-- | This module defines the structure of a TI-85 backup file.
-- A backup file contains system memory (data section 1), user
-- memory (section 2), and a variable table that maps where
-- user variables are located in data section 2.
module Data.TI85.File.Backup where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8,Word16)

import Data.TI85.File.Variable (VarType)

-- | An entry in the variable table.
-- Note: the variable table is stored in
-- reverse byte order (including 2-byte
-- words!).
data VarTableEntry = VarTableEntry {
    entryId :: Word8,
    entryAddr :: Word16,
    entryNameLen :: Word8,
    entryName :: ByteString
    } deriving Show

-- | A list of variable table entries
type VarTable = [VarTableEntry]

-- | Backup-specific header. This header comes after
-- the more general `Data.TI85.File.TIHeader`.
data TIBackupHeader = TIBackupHeader {
    hdrDataLenOffset :: Word16, -- Always 9
    hdrData1Len :: Word16,
    hdrTypeID :: Word8, -- Always 0x1D
    hdrData2Len :: Word16,
    hdrData3Len :: Word16,
    hdrData2Addr :: Word16
    } deriving Show

-- | The top-level structure of a backup
-- file.
data TIBackupData = TIBackupData {
    backupHeader :: TIBackupHeader,
    data1Len :: Word16,
    data1 :: ByteString,
    data2Len :: Word16,
    data2 :: ByteString,
    varTableLen :: Word16,
    varTable :: VarTable
    } deriving Show

