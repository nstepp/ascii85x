
-- | A collection of types and utilities for dealing with
-- TI Link backup files.
module Data.TI85.File (
    module Data.TI85.File.Variable,
    module Data.TI85.File.Backup,
    TIFile(..)
    ) where

import Data.TI85.File.Variable
import Data.TI85.File.Backup

-- | There are two possible file formats
-- * Variable files contain one or more variables,
--   encoded in a variable-specific way.
-- * Backup files contain memory dumps, which also
--   include variable data in a raw form.
data TIFile = BackupFile TIBackupFile | VariableFile TIVarFile
