module Main where

import Prelude hiding (putStrLn)
import Control.Monad
import Data.List (isSuffixOf)
import Data.Text (Text, pack, intercalate, unpack)
import Data.Text.Encoding (decodeLatin1)
import Data.Text.IO (putStrLn)
import Data.ByteString (ByteString, foldr')
import Data.Word
import Data.Version
import Numeric (showHex)

import Options.Applicative

import Data.TI85
import Paths_ascii85x (version)

import System.Exit (exitSuccess)

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

processVarData :: Config -> TIVarData -> IO ()
processVarData args (TIVarData tiVars) = do
    let vars = map readVariable tiVars
    when (debug args) $ print vars
    let names = map (tiDecode.varName) tiVars
    let types = map (showType.idToType.varId) tiVars
    forM_ (zip3 names vars types) $ \(name,var,varType) -> do
        putStrLn $ "\n" <> varType <> " \"" <> name <> "\":"
        printVariable var

processBackupData :: Config -> TIBackupData -> IO ()
processBackupData args tiBackup = do
    let backupHdr = backupHeader tiBackup
    let data2Addr = hdrData2Addr backupHdr
    let dataDisplay = if verbose args
        then print . foldr' hexify ""
        else print
    putStrLn $ pack $ "Data Section 1 (" <> show (data1Len tiBackup) <> "):"
    dataDisplay (data1 tiBackup)
    putStrLn $ pack $ "Data Section 2 (" <> show (data2Len tiBackup) <> "):"
    dataDisplay (data2 tiBackup)
    putStrLn $ pack $ "Data 2 Address: " <> showHex data2Addr "\n"
    putStrLn $ pack $ "Variable Table (" <> show (varTableLen tiBackup) <> "):"
    printVariableTable data2Addr (varTable tiBackup)
  where
    hexify :: Word8 -> ShowS
    hexify w = 
        let pad = if w < 0xf then "0" else ""
        in \s' -> pad <> showHex w s'

main :: IO ()
main = do
    args <- execParser argInfo

    tiFile <- readTIFile (programFile args)

    when (showInfo args) $ do
        printFileSummary tiFile
        exitSuccess

    case tiData tiFile of
        BackupData backup -> processBackupData args backup
        VariableData vars -> processVarData args vars

