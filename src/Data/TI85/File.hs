module Data.TI85.File (
    module Data.TI85.File.Variable,
    module Data.TI85.File.Backup,
    TIFile(..)
    ) where

import Data.TI85.File.Variable
import Data.TI85.File.Backup

data TIFile = BackupFile TIBackupFile | VariableFile TIVarFile
