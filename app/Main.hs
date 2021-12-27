module Main where

import Prelude hiding (putStrLn)
import Control.Monad
import Data.List (isSuffixOf)
import Data.Text (Text, pack, intercalate, unpack)
import Data.Text.Encoding (decodeUtf8, decodeLatin1)
import Data.Text.IO (putStrLn)
import Data.ByteString (ByteString)
import Data.Word
import Data.Version

import Options.Applicative

import Data.TI85
import Paths_ascii85x (version)

import System.Exit (exitSuccess)
import Numeric (showHex)

data Config = Config {
    showInfo :: Bool,
    debug :: Bool,
    verbose :: Bool,
    programFile :: FilePath
    }

versionOpt = infoOption (showVersion version) (
    long "version"
    <> short 'V'
    <> help "Show version only"
    )

argParser :: Parser Config
argParser = Config
    <$> switch (
        long "info"
        <> short 'i'
        <> help "Show file info only"
        )
    <*> switch (
        long "debug"
        <> short 'D'
        <> help "Show extra variable details"
        )
    <*> switch (
        long "verbose"
        <> short 'v'
        <> help "Show variable file summary"
        )
    <*> strArgument (
        metavar "VARFILE"
        <> help "85x variable file"
        )

argInfo = info (argParser <**> versionOpt <**> helper) (
    fullDesc <> progDesc "Convert TI-85 variable files to text"
    )

tiFileSummary :: TIVarFile -> Text
tiFileSummary tiFile =
    let hdr = Data.TI85.header tiFile
        check = checksum tiFile
        sig = decodeLatin1 $ hdrSig hdr
        comment = decodeLatin1 $ hdrComment hdr
        vars = varsData tiFile
    in
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

processVarFile :: Config -> FilePath -> IO ()
processVarFile args filename = do
    (varFile, vars) <- readVariableFile filename
    when (verbose args) $ putStrLn (tiFileSummary varFile)
    when (debug args) $ print vars
    let tiVars = varsData varFile
    let names = map (tiDecode.varName) tiVars
    let types = map (showType.idToType.varId) tiVars
    forM_ (zip3 names vars types) $ \(name,var,varType) -> do
        putStrLn $ "\n" <> varType <> " \"" <> name <> "\":"
        printVariable var

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

processBackupFile :: Config -> FilePath -> IO ()
processBackupFile args filename = do
    tiBackup <- readTIBackupFile filename
    let hdr = tiHeader tiBackup
    let comment = decodeLatin1 (hdrComment hdr)
    let backupHdr = backupHeader tiBackup
    let data2Addr = hdrData2Addr backupHdr
    putStrLn $ "Backup File: " <> comment
    putStrLn $ pack $ "Data Section 1 (" <> show (data1Len tiBackup) <> "):"
    print $ data1 tiBackup
    putStrLn $ pack $ "Data Section 2 (" <> show (data2Len tiBackup) <> "):"
    print $ data2 tiBackup
    putStrLn $ pack $ "Data 2 Address: " <> showHex data2Addr "\n"
    putStrLn $ pack $ "Variable Table (" <> show (varTableLen tiBackup) <> "):"
    printVariableTable data2Addr (varTable tiBackup)

main :: IO ()
main = do
    args <- execParser argInfo

    when (showInfo args) $ do
        varFile <- readTIVarFile (programFile args)
        putStrLn (tiFileSummary varFile)
        exitSuccess
    let filename = programFile args
    if ".85b" `isSuffixOf` filename || ".85B" `isSuffixOf` filename
        then processBackupFile args filename
        else processVarFile args filename

