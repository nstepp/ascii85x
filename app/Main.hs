{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStrLn)
import Control.Monad
import Data.Text (Text, pack, intercalate, unpack)
import Data.Text.Encoding (decodeUtf8, decodeLatin1)
import Data.Text.IO (putStrLn)
import Data.ByteString (ByteString)

import Options.Applicative

import Data.TI85
import System.Exit (exitSuccess)

data Config = Config {
    showInfo :: Bool,
    debug :: Bool,
    verbose :: Bool,
    programFile :: FilePath
    }

versionOpt = infoOption (unpack version) (
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

main :: IO ()
main = do
    args <- execParser argInfo

    when (showInfo args) $ do
        varFile <- readTIVarFile (programFile args)
        putStrLn (tiFileSummary varFile)
        exitSuccess

    (varFile, vars) <- readVariableFile (programFile args)
    when (verbose args) $ putStrLn (tiFileSummary varFile)
    when (debug args) $ print vars
    let tiVars = varsData varFile
    let names = map (tiDecode.varName) tiVars
    let types = map (showType.idToType.varId) tiVars
    forM_ (zip3 names vars types) $ \(name,var,varType) -> do
        putStrLn $ "\n" <> varType <> " \"" <> name <> "\":"
        printVariable var

