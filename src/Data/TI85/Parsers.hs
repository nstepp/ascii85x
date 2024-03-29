{-# LANGUAGE DataKinds #-}

-- | This module contains the highest-level user-facing functions.
-- Typically, code using this library will want to call `readTIFile`
-- on a file name.
module Data.TI85.Parsers (
    -- * General TI Files
    readTIFile,
    parseTIHeader,
    parseTIFile,
    -- * Backup Files
    -- ** High-level Parsers
    parseTIBackupHeader,
    readUserMem,
    -- ** Lower-level Parsers
    readVarMem,
    extractVar,
    -- * Variable Files
    -- ** High-level Parsers
    readVariable,
    parseVariable,
    -- ** Lower-level Parsers
    parseProgram,
    parseTINumber,
    parseToken
    ) where

import Prelude hiding (take,takeWhile,putStrLn)
import Data.Char
import Data.Bits
import Data.Word
import Data.Array.Unboxed (Array, UArray, array, listArray, (!))
import Data.ByteString (ByteString, foldr')
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator (lookAhead)
import Control.Applicative
import Control.Monad (guard, when, forM)

import Data.TI85.Var
import Data.TI85.Var.Pic
import Data.TI85.Encoding
import Data.TI85.Token
import Data.TI85.File

bytes2Int :: ByteString -> Int
bytes2Int = BS.foldr' (\w x -> 256*x + fromEnum w) 0

anyWord16 :: Parser Word16
anyWord16 = do
    bytes <- take 2
    let val = bytes2Int bytes
    return (toEnum val)

-- | Parse a general TI file, which might be
-- a variable file or backup file.
parseTIFile :: Parser TIFile
parseTIFile = do
    header <- parseTIHeader
    (checksum, calcSum) <- lookAhead (calculateSum $ hdrDataLen header)

    when (calcSum /= checksum) (error "Checkum check failed")

    contents <- 
        BackupData <$> parseTIBackupData
        <|> VariableData <$> parseTIVarData

    return $ TIFile {
        tiHeader = header,
        tiData = contents,
        tiChecksum = checksum
        }
  where
    calculateSum :: Word16 -> Parser (Word16,Word16)
    calculateSum len = do
        bytes <- take (fromEnum len)
        let sum = foldr' (\w s -> fromEnum w + s) (0::Int) bytes
        checksum <- anyWord16
        return (checksum, toEnum $ sum .&. 0xffff)

-- | Read a general TI file, which might be
-- a variable file or backup file.
readTIFile :: FilePath -> IO TIFile
readTIFile fileName = do
    contents <- BS.readFile fileName
    let tiFile = parseOnly parseTIFile contents
    either error return tiFile

-- | The backup header is a second header after
-- the top-level file header (`TIFile`).
-- It contains sizes of the three data sections
-- and the location of user memory in RAM (which
-- can use used with the variable table to look
-- up variable data).
parseTIBackupHeader :: Parser TIBackupHeader
parseTIBackupHeader = do
    dataOffset <- string "\x09\x00"
    len1 <- anyWord16
    typeId <- word8 0x1d
    len2 <- anyWord16
    len3 <- anyWord16
    addr <- anyWord16
    return $ TIBackupHeader {
        hdrDataLenOffset = 0x9,
        hdrData1Len = len1,
        hdrTypeID = typeId,
        hdrData2Len = len2,
        hdrData3Len = len2,
        hdrData2Addr = addr
        }

parseVarTableEntry :: Parser VarTableEntry
parseVarTableEntry = do
    entId <- anyWord8
    entAddr <- anyWord16
    len <- anyWord8
    name <- take (fromEnum len)
    return $ VarTableEntry {
        entryId = entId,
        entryAddr = entAddr,
        entryNameLen = len,
        entryName = name
        }

parseTIBackupData :: Parser TIBackupData
parseTIBackupData = do
    backupHdr <- parseTIBackupHeader
    len1 <- anyWord16
    data1 <- take $ fromEnum len1
    len2 <- anyWord16
    data2 <- take $ fromEnum len2
    len3 <- anyWord16
    data3 <- take $ fromEnum len3
    let vars = parseOnly (many' parseVarTableEntry) (BS.reverse data3)
    return $ TIBackupData {
        backupHeader = backupHdr,
        data1Len = len1,
        data1 = data1,
        data2Len = len2,
        data2 = data2,
        varTableLen = len3,
        varTable = either error id vars
        }

-- |The TI-85 header is common between backup files
-- and variable files.
--
-- +--------+---+---------------------------------+
-- | 8 Byte | 3 | 42 Byte                         |
-- +========+===+=================================+
-- |**TI85**|xyz| Comment                         |
-- +--------+---+---------------------------------+
--
-- where @xyz@ is always @0x1a,0x0c,0x00@.
parseTIHeader :: Parser TIHeader
parseTIHeader = do
    string "**TI85**"
    string "\x1a\x0c\x00"
    rawComment <- take 42
    dataLen <- anyWord16
    return $ TIHeader {
        hdrSig = "**TI85**",
        hdrSig2 = "\x1a\x0c\x00",
        hdrComment = BS.takeWhile (/= 0x0) rawComment,
        hdrDataLen = dataLen
        }

parseTIVar :: Parser TIVar
parseTIVar = do
    offset <- anyWord16
    len <- anyWord16
    varType <- anyWord8
    nameLen <- anyWord8
    name <- take (fromEnum nameLen)
    dataLen <- anyWord16
    var <- take (fromEnum dataLen)
    return $ TIVar {
       varOffset = offset,
       varLen = len,
       varId = varType,
       varNameLen = nameLen,
       varName = name,
       varDataLen = dataLen,
       varData = var
       }

parseTIVarData :: Parser TIVarData
parseTIVarData = TIVarData <$> many1' parseTIVar

-- |Programs are either plain text, ot encoded in a
-- tokenized format. See "Data.TI85.Token" for the 
-- mapping.
parseProgram :: Parser Program
parseProgram = do
    len <- fromEnum <$> anyWord16
    encoding <- peekWord8'
    case encoding of
        0x00 -> do
            anyWord8
            PlainText <$> parsePlaintext (len-1)
        _ -> Tokenized <$> parseTokenized len

parsePlaintext :: Int -> Parser Text
parsePlaintext len = do
    bytes <- take len
    return $ tiDecode bytes

parseTokenized :: Int -> Parser [Token]
parseTokenized len = do
    bytes <- take len
    let tokenResult = parseOnly (many' parseToken) bytes
    either error return tokenResult
        
-- |Interpret data as a token (or token stream)
parseToken :: Parser Token
parseToken = do
    tokenByte <- anyWord8
    let token = tokenTable ! tokenByte
    tokenText <- case token of
        Invalid -> return T.empty
        Fixed t -> return t
        QuoteText -> do
            t <- zeroTerminated
            return $  '"' `T.cons` t `T.snoc` '"'
        NameLength -> varLength
        FixedLength n -> tiDecode <$> take n
        Extended -> do
            extToken <- anyWord8
            case tokenTableExtended ! extToken of
                Fixed ext -> return ext
                _ -> return T.empty
        Conversion -> do
            unit1 <- varLength
            unit2 <- varLength
            return $ T.intercalate "->" [unit1,unit2]
        Literal -> zeroTerminated
        Label -> do
            label <- zeroTerminated
            return $ "Lbl " <> label
        Goto -> do
            mystery <- anyWord16
            label <- zeroTerminated
            return $ "Goto " <> label
    return $ Token token tokenText
  where
    zeroTerminated = do
        bytes <- takeWhile (/= 0x0)
        zero <- word8 0x0
        return $ tiDecode bytes
    varLength = do
        len <- anyWord8
        bytes <- take (fromEnum len)
        return $ tiDecode bytes

bcd :: ByteString -> Double
bcd bytes = snd $ BS.foldl' accDecimal (0,0.0) bytes
  where
    accDecimal :: (Int, Double) -> Word8 -> (Int, Double)
    accDecimal (place, value) byte =
        let (high,low) = toNibbles byte
            highVal = fromIntegral high * 10.0 ** fromIntegral place
            lowVal = fromIntegral low * 10.0 ** fromIntegral (place-1)
        in (place-2, value+highVal+lowVal)
    toNibbles :: Word8 -> (Word8,Word8)
    toNibbles byte =
        let low = byte .&. 0x0f
            high = shiftR byte 4 .&. 0x0f
        in (high,low)

-- |Numeric values can either be real or complex, and most variable
-- types can store either.
parseTINumber :: Parser TINumber
parseTINumber = do
    (realFlags, realPart) <- parseReal
    let isComplex = testBit realFlags 0
    if isComplex
        then do
            (_imagFlags, imagPart) <- parseReal
            return $ TIComplex realPart imagPart
        else return $ TIReal realPart
  where
    parseReal :: Parser (Word8, Double)
    parseReal = do
        flags <- anyWord8
        expRaw <- anyWord16
        let exp = fromEnum expRaw - 0xFC00
        mantissa <- bcd <$> take 7
        let sign = testBit flags 7
        let value = mantissa * 10 ** fromIntegral exp
        let signedValue = if sign
            then -1.0 * value
            else value
        return (flags,signedValue)

parsePicture :: Parser TIBitmap
parsePicture = do
    word8 0xf0
    word8 0x03
    pic <- take 1008
    case fromBytes pic of
        Just tiPic -> return tiPic
        Nothing -> fail "Wrong size of picture bitmap"

-- |Parser for the elements contained a variable file
-- that has possibly more than one 
parseVariable :: VarType -> Parser Variable
parseVariable (VarValue _) = TIScalar <$> parseTINumber
parseVariable (VarVector _) = do
    alwaysOne <- anyWord8
    len <- fromEnum <$> anyWord8
    vals <- count len parseTINumber
    return $ TIVector vals
parseVariable (VarList _) = do
    len <- fromEnum <$> anyWord16
    vals <- count len parseTINumber
    return $ TIList vals
parseVariable (VarMatrix _) = do
    numCols <- fromEnum <$> anyWord8
    numRows <- fromEnum <$> anyWord8
    vals <- count numRows (count numCols parseTINumber)
    return $ TIMatrix vals
parseVariable (VarConstant _) = TIConstant <$> parseTINumber
parseVariable VarEquation = do
    len <- fromEnum <$> anyWord16
    tokens <- parseTokenized len
    let tokenText = map (\(Token _ t) -> t) tokens
    let eqText = T.concat tokenText
    return $ TIEquation eqText
parseVariable VarString = do
    len <- fromEnum <$> anyWord16
    TIString <$> parsePlaintext len
parseVariable VarProgram = TIProgram <$> parseProgram
parseVariable VarPicture = TIPicture <$> parsePicture
parseVariable VarSettingsFunc = TIFuncSettings <$> parseFuncSettings
parseVariable VarSettingsPolar = TIPolarSettings <$> parsePolarSettings
parseVariable VarSettingsParam = TIParamSettings <$> parseParamSettings
parseVariable VarSettingsDiff = TIDiffEqSettings <$> parseDiffEqSettings
parseVariable VarSavedWinSize = TIZRCL <$> parseWinSettings
parseVariable VarGDBFunc = TIFuncGDB <$> parseFuncGDB
parseVariable VarUnknown = return $ TIString "?"
parseVariable _ = return $ TIString "(not implemented)"

parseModeSettings :: Parser ModeSettings
parseModeSettings = do
    modeByte <- anyWord8
    let drawDot = testBit modeByte 0
        simulG = testBit modeByte 1
        gridOn = testBit modeByte 2
        polarGC = testBit modeByte 3
        coordOff = testBit modeByte 4
        axesOff = testBit modeByte 5
        labelOb = testBit modeByte 6
    return $ ModeSettings {
        modeDrawDot = drawDot,
        modeSimulG = simulG,
        modeGridOn = gridOn,
        modePolarGC = polarGC,
        modeCoordOff = coordOff,
        modeAxesOff = axesOff,
        modeLabelOn = labelOb
        }

parseFuncGDB :: Parser (GDB Func)
parseFuncGDB = do
    len <- anyWord16
    mode <- parseModeSettings
    settings <- parseBareFuncSettings
    numFunc <- fromEnum <$> satisfy (<=99)
    lib <- count numFunc parseFuncEntry
    return $ GDB {
        gdbMode = mode,
        gdbSettings = settings,
        gdbLib = lib
        }

parseFuncEntry :: Parser (GDBLibEntry Func)
parseFuncEntry = do
    idByte <- anyWord8
    let selected = testBit idByte 7
        funcId = idByte .&. 0x7f
    (TIEquation eqn) <- parseVariable VarEquation
    return $ GDBLibEntry {
        libId = fromEnum funcId,
        libSelected = selected,
        libEqn = eqn
        }
{-
0 	2 bytes 	Length, in bytes, of GDB, minus two.
2 	1 byte 	Mode settings (see mode setting table below)
3 	10 bytes 	A real number: xMin
13 (Dh) 	10 bytes 	A real number: xMax
23 (17h) 	10 bytes 	A real number: xScl
33 (21h) 	10 bytes 	A real number: yMin
43 (2Bh) 	10 bytes 	A real number: yMax
53 (35h) 	10 bytes 	A real number: yScl
63 (3Fh) 	10 bytes 	A real number: xRes
73 (49h) 	1 byte 	Number of functions defined (up to 99)
74 (4Ah) 	n bytes 	Function table - several function definitions one after another (see function table below)
-}

parseBareFuncSettings :: Parser FuncSettings
parseBareFuncSettings = do
    FuncSettings
        <$> parseTINumber -- fXMin
        <*> parseTINumber -- fXMax
        <*> parseTINumber -- fXScl
        <*> parseTINumber -- fYMin
        <*> parseTINumber -- fYMax
        <*> parseTINumber -- fYScl

parseFuncSettings :: Parser FuncSettings
parseFuncSettings = do
    word8 0x51
    word8 0x00
    anyWord8
    val <- parseBareFuncSettings
    take 20
    return val

parsePolarSettings :: Parser PolarSettings
parsePolarSettings = do
    word8 0x5b
    word8 0x00
    anyWord8
    PolarSettings
        <$> parseTINumber -- polThetaMin
        <*> parseTINumber -- polThetaMax
        <*> parseTINumber -- polThetaStep
        <*> parseTINumber -- polXMin
        <*> parseTINumber -- polXMax
        <*> parseTINumber -- polXScl
        <*> parseTINumber -- polYMin
        <*> parseTINumber -- polYMax
        <*> parseTINumber -- polYScl

parseParamSettings :: Parser ParamSettings
parseParamSettings = do
    word8 0x5b
    word8 0x00
    anyWord8
    ParamSettings
        <$> parseTINumber -- parTMin
        <*> parseTINumber -- parTMax
        <*> parseTINumber -- parTStep
        <*> parseTINumber -- parXMin
        <*> parseTINumber -- parXMax
        <*> parseTINumber -- parXScl
        <*> parseTINumber -- parYMin
        <*> parseTINumber -- parYMax
        <*> parseTINumber -- parYScl

parseDiffEqSettings :: Parser DiffEqSettings
parseDiffEqSettings = do
    word8 0x71
    word8 0x00
    anyWord8
    DiffEqSettings
        <$> parseTINumber -- diffTol
        <*> parseTINumber -- diffTPlot
        <*> parseTINumber -- diffTMin
        <*> parseTINumber -- diffTMax
        <*> parseTINumber -- diffTStep
        <*> parseTINumber -- diffXMin
        <*> parseTINumber -- diffXMax
        <*> parseTINumber -- diffXScl
        <*> parseTINumber -- diffYMin
        <*> parseTINumber -- diffYMax
        <*> parseTINumber -- diffYScl
        <*> parseAxis -- diffXAxis
        <*> parseAxis -- diffYAxis
  where
    parseAxis :: Parser DiffEqAxis
    parseAxis = do
        axisCode <- anyWord8
        return $ case axisCode of
            0x0 -> AxisT
            w | w >= 0x10 && w <= 0x19 ->
                AxisQ (toEnum (fromEnum w - 0x10))
            w | w >= 0x20 && w <= 0x29 ->
                AxisQ' (toEnum (fromEnum w - 0x20))
            _ -> error "Invalid axis code"

parseWinSettings :: Parser SavedWinSettings
parseWinSettings = do
    word8 0x82
    word8 0x00
    SavedWinSettings
        <$> parseTINumber -- zThetaMin
        <*> parseTINumber -- zThetaMax
        <*> parseTINumber -- zThetaStep
        <*> parseTINumber -- ztPlot
        <*> parseTINumber -- ztMin
        <*> parseTINumber -- ztMax
        <*> parseTINumber -- ztStep
        <*> parseTINumber -- zxMin
        <*> parseTINumber -- zxMax
        <*> parseTINumber -- zxScl
        <*> parseTINumber -- zyMin
        <*> parseTINumber -- zyMax
        <*> parseTINumber -- zyScl

parseUserMem :: Word16 -> VarTable -> Parser [Variable]
parseUserMem baseAddr vars = do
    forM vars (extractVar baseAddr)

-- | Parse a variable out of user memory, without
-- consuming any input (hence "extract"). This allows
-- many calls on the same memory. The base address
-- is required for converting the variable addresses
-- in the table to offsets into the user memory backup.
extractVar :: Word16 -> VarTableEntry -> Parser Variable
extractVar baseAddr (VarTableEntry idNum addr _ name) = do
    let offset = fromEnum $ addr - baseAddr
    lookAhead $ take offset *> parseVariable (idToType idNum)

-- | Given a bytestring of user memory, look up the variable
-- table entry and return a `Variable`.
readVarMem :: Word16 -> VarTableEntry -> ByteString -> Variable
readVarMem baseAddr entry userData =
    let var = parseOnly (extractVar baseAddr entry) userData
    in either error id var

-- | Read all ofthe variables contained in a user memory
-- backup, according to a variable table.
readUserMem :: Word16 -> VarTable -> ByteString -> [Variable]
readUserMem baseAddr table userData =
    let vars = parseOnly (parseUserMem baseAddr table) userData
    in either error id vars

-- |Convert raw variable data into a Variable.
readVariable :: TIVar -> Variable
readVariable var =
    let varType = idToType (varId var)
        parsedVar = parseOnly (parseVariable varType) (varData var)
    in either error id parsedVar

