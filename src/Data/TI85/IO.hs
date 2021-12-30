module Data.TI85.IO where

import Prelude hiding (putStrLn)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text, intercalate, pack)
import Data.Text.Encoding (decodeLatin1)
import Data.Text.IO (putStrLn)
import Data.Word
import Numeric (showHex)

import Data.TI85.Encoding
import Data.TI85.File
import Data.TI85.Parsers
import Data.TI85.Var


printVariableTable :: Word16 -> VarTable -> IO ()
printVariableTable baseAddr vars = do
    forM_ vars printEntry
  where
    printEntry (VarTableEntry idNum addr _ name) = do
        let idName = (showType.idToType) idNum
        let offset = addr - baseAddr 
        putStrLn $ "Name: " <> tiDecode name <> "\n" <>
            "Type: " <> idName <> "\n" <>
            "Addr: " <> pack (showHex addr "") <> " (offset " <> (pack.show) offset <> ")\n"

extractVariableTable :: Word16 -> TIBackupData -> IO ()
extractVariableTable baseAddr tiBackup = do
    let table = varTable tiBackup
    let userData = data2 tiBackup
    let vars = readUserMem baseAddr table userData
    forM_ table $ \entry@(VarTableEntry idNum addr _nameLen name) -> do
        let var = readVarMem baseAddr entry userData
        putStrLn $ "\n" <> (showType . idToType ) idNum <> " \"" <> tiDecode name <> "\""
            <> " at 0x" <> hexify addr <> " (0x" <> hexify (addr-baseAddr) <> "):\n"
        printVariable var
  where
    hexify :: Word16 -> Text
    hexify w = pack (showHex w "")

printFileSummary :: TIFile -> IO ()
printFileSummary tiFile =
    let hdr = tiHeader tiFile
        check = tiChecksum tiFile
        sig = decodeLatin1 $ hdrSig hdr
        comment = decodeLatin1 $ hdrComment hdr
        fileType = case tiData tiFile of
            BackupData _ -> "Backup"
            VariableData _ -> "Variable"
    in do
        putStrLn $
            "\nTI " <> fileType <> " File <" <> sig <> ">\n" <>
            "\"" <> comment <> "\"\n\n"
        case tiData tiFile of
            BackupData backupData -> printBackupSummary backupData
            VariableData variableData -> printVariableSummary variableData

printBackupSummary :: TIBackupData -> IO ()
printBackupSummary tiBackup = do
    let backupHdr = backupHeader tiBackup
    let data2Addr = hdrData2Addr backupHdr
    putStrLn $ pack $ "Data Section 1 (" <> show (data1Len tiBackup) <> "):"
    putStrLn $ pack $ "Data Section 2 (" <> show (data2Len tiBackup) <> "):"
    putStrLn $ pack $ "Data 2 Address: " <> showHex data2Addr "\n"

printVariableSummary :: TIVarData -> IO ()
printVariableSummary (TIVarData vars) =
    putStrLn $
        "Variables:\n" <>
        intercalate "\n" (map varSummary vars)
  where
    varSummary :: TIVar -> Text
    varSummary var =
        let varIdStr = (pack.show.fromEnum) (varId var)
            varIdType = (showType.idToType) (varId var)
        in  "\tName: " <> tiDecode (varName var) <> "\n" <>
            "\tType: " <> varIdType <> " (" <> varIdStr <> ")\n" <>
            "\tLength : " <> (pack.show.fromEnum) (varDataLen var) <> "\n"


