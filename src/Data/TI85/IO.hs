module Data.TI85.IO where

import Prelude hiding (putStrLn)
import Control.Monad
import Data.Text (Text, intercalate, pack)
import Data.Text.Encoding (decodeLatin1)
import Data.Text.IO (putStrLn)
import Data.Word
import Numeric (showHex)

import Data.TI85.Encoding
import Data.TI85.BackupFile
import Data.TI85.VarFile


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

printFileSummary :: TIVarFile -> IO ()
printFileSummary tiFile =
    let hdr = header tiFile
        check = checksum tiFile
        sig = decodeLatin1 $ hdrSig hdr
        comment = decodeLatin1 $ hdrComment hdr
        vars = varsData tiFile
    in putStrLn $
        "\nTI Variable File <" <> sig <> ">\n" <>
        "\"" <> comment <> "\"\n\n" <>
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


