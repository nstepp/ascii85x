
-- | A collection of types and utilities for dealing with
-- TI Link backup files.
module Data.TI85.File (
    module Data.TI85.File.Variable,
    module Data.TI85.File.Backup,
    TIHeader(..),
    TIFileData(..),
    TIFile(..)
    ) where

import Data.ByteString
import Data.Word

import Data.TI85.File.Variable
import Data.TI85.File.Backup

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

-- | There are two possible file formats
-- * Variable files contain one or more variables,
--   encoded in a variable-specific way.
-- * Backup files contain memory dumps, which also
--   include variable data in a raw form.
data TIFileData = BackupData TIBackupData
    | VariableData TIVarData
    deriving Show

data TIFile = TIFile {
    tiHeader :: TIHeader,
    tiData :: TIFileData,
    tiChecksum :: Word16
    } deriving Show

