{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specifics of the TI-85 variables themselves
-- (i.e. not their representation in the file).
module Data.TI85.Var (
    -- * Types
    Variable(..),
    TINumber(..),
    -- ** Program
    Program(..),
    Token(..),
    -- ** Window Settings
    FuncSettings(..),
    PolarSettings(..),
    ParamSettings(..),
    SavedWinSettings(..),
    -- *** Differential Equations
    DiffEqSettings(..),
    DiffEqAxis(..),
    AxisInd(..),
    -- ** Graph Database
    ModeSettings(..),
    GraphMode(..),
    FuncEqn(..),
    ParamEqn(..),
    DiffEqEqn(..),
    GDBLibEntry(..),
    GDBEqn(..),
    GDBSettings(..),
    GDB(..),
    HasGDB,

    -- * Text Conversion
    showVariable,
    -- ** Variable-specific
    showNumber,
    showProgram,
    showFuncSettings,
    showPolarSettings,
    showParamSettings,
    showDiffEqSettings,
    showWinSettings,
    showGDB,
    showGDBMode,

    -- * IO
    printVariable
    ) where

import Prelude hiding (concat, putStrLn)
import Data.Text (Text, concat, pack, intercalate)
import Data.Text.IO (putStrLn)
import Data.TI85.Token (TokenDef)
import Data.TI85.Var.Pic

-- * Types

-- | An instance of a token from the `TokenDef`
-- table. This will include actual text that goes
-- along with a token, when it is not a fixed-text
-- token.
data Token = Token TokenDef Text deriving Show

-- | A program is either stored as plaintext
-- (in the TI-86 codepage; see `Data.TI85.Encoding.tiDecode`)
-- or a list of tokens. The two are represented here to
-- maintain that information.
data Program = PlainText Text
    | Tokenized [Token]
    deriving Show

-- | Numerical variables are either Real or Complex.
data TINumber = TIReal Double | TIComplex Double Double deriving Show

-- ** Window Settings

-- | Function window settings
data FuncSettings = FuncSettings {
    fXMin :: TINumber,
    fXMax :: TINumber,
    fXScl :: TINumber,
    fYMin :: TINumber,
    fYMax :: TINumber,
    fYScl :: TINumber
    }
    deriving Show

-- | Polar window settings
data PolarSettings = PolarSettings {
    polThetaMin :: TINumber,
    polThetaMax :: TINumber,
    polThetaStep :: TINumber,
    polXMin :: TINumber,
    polXMax :: TINumber,
    polXScl :: TINumber,
    polYMin :: TINumber,
    polYMax :: TINumber,
    polYScl :: TINumber
    }
    deriving Show

-- | Parametric window settings
data ParamSettings = ParamSettings {
    parTMin :: TINumber,
    parTMax :: TINumber,
    parTStep :: TINumber,
    parXMin :: TINumber,
    parXMax :: TINumber,
    parXScl :: TINumber,
    parYMin :: TINumber,
    parYMax :: TINumber,
    parYScl :: TINumber
    }
    deriving Show

-- | Differential equation axes can come
-- with an index (e.g. Q1-Q9).
data AxisInd = Axis0
    | Axis1
    | Axis2
    | Axis3
    | Axis4
    | Axis5
    | Axis6
    | Axis7
    | Axis8
    | Axis9
    deriving (Show,Eq,Enum)

-- | Differential equation axis type
data DiffEqAxis = AxisT
    | AxisQ AxisInd
    | AxisQ' AxisInd
    deriving Show

-- | Differential equation window settings
data DiffEqSettings = DiffEqSettings {
    diffTol :: TINumber,
    diffTPlot :: TINumber,
    diffTMin :: TINumber,
    diffTMax :: TINumber,
    diffTStep :: TINumber,
    diffXMin :: TINumber,
    diffXMax :: TINumber,
    diffXScl :: TINumber,
    diffYMin :: TINumber,
    diffYMax :: TINumber,
    diffYScl :: TINumber,
    diffXAxis :: DiffEqAxis,
    diffYAxis :: DiffEqAxis
    }
    deriving Show

-- | Saved window settings, used for ZRCL.
data SavedWinSettings = SavedWinSettings {
    zThetaMin :: TINumber,
    zThetaMax :: TINumber,
    zThetaStep :: TINumber,
    ztPlot :: TINumber,
    ztMin :: TINumber,
    ztMax :: TINumber,
    ztStep :: TINumber,
    zxMin :: TINumber,
    zxMax :: TINumber,
    zxScl :: TINumber,
    zyMin :: TINumber,
    zyMax :: TINumber,
    zyScl :: TINumber
    }
    deriving Show

data ModeSettings = ModeSettings {
    modeDrawDot :: Bool,
    modeSimulG :: Bool,
    modeGridOn :: Bool,
    modePolarGC :: Bool,
    modeCoordOff :: Bool,
    modeAxesOff :: Bool,
    modeLabelOn :: Bool
    }
    deriving Show

-- | There are four graph modes, each with
-- its own set of window ranges and equation
-- types.
data GraphMode = Func | Polar | Param | DiffEq

-- | Plain functions and Polar functions both use
-- a single equation.
type FuncEqn = Text

-- | Parametric functions use a pair of equations
data ParamEqn = ParamEqn {
    xEqn :: Text,
    yEqn :: Text
    } deriving Show

-- | Differential equations have a single equation
-- paired with an initial condition.
data DiffEqEqn = DiffEqEqn {
    diffEqn :: Text,
    diffIC :: Double
    } deriving Show

class HasGDB (a :: GraphMode) where
    type GDBEqn a :: *
    type GDBSettings a :: *

    showGDBSettings :: GDB a -> Text
    showGDBHeader :: GDB a -> Text

    showGDBLib :: GDB a -> Text
    showGDBLib gdb = intercalate "\n" (map showGDBEntry (gdbLib gdb))

    showGDBEntry :: GDBLibEntry a -> Text

instance HasGDB Func where
    type GDBEqn Func = FuncEqn
    type GDBSettings Func = FuncSettings

    showGDBSettings gdb = showFuncSettings (gdbSettings gdb)
    showGDBHeader _ = "ID\tSelected\tEquation\n"
    showGDBEntry (GDBLibEntry entryId selected eqn) =
        showText entryId <> "\t"
        <> showText selected <> "\t"
        <> eqn

instance HasGDB Polar where
    type GDBEqn Polar = FuncEqn
    type GDBSettings Polar = PolarSettings
    showGDBSettings gdb = showPolarSettings (gdbSettings gdb)
    showGDBHeader _ = "ID\tSelected\tEquation\n"
    showGDBEntry (GDBLibEntry entryId selected eqn) =
        showText entryId <> "\t"
        <> showText selected <> "\t"
        <> eqn

instance HasGDB Param where
    type GDBEqn Param = ParamEqn
    type GDBSettings Param = ParamSettings
    showGDBSettings gdb = showParamSettings (gdbSettings gdb)
    showGDBHeader _ = "ID\tSelected\tx-Equation\ty-Equation\n"
    showGDBEntry (GDBLibEntry entryId selected eqn) =
        showText entryId <> "\t"
        <> showText selected <> "\t"
        <> xEqn eqn <> "\t"
        <> yEqn eqn

instance HasGDB DiffEq where
    type GDBEqn DiffEq = DiffEqEqn
    type GDBSettings DiffEq = DiffEqSettings
    showGDBSettings gdb = showDiffEqSettings (gdbSettings gdb)
    showGDBHeader _ = "ID\tSelected\tEquation\tInitial Condition\n"
    showGDBEntry (GDBLibEntry entryId selected eqn) =
        showText entryId <> "\t"
        <> showText selected <> "\t"
        <> diffEqn eqn <> "\t"
        <> showText (diffIC eqn)


-- | A graph database entry, containing a
-- function ID, whether or not it is currently
-- selected, and the equations that define the
-- function.
data GDBLibEntry (a :: GraphMode) where
    GDBLibEntry :: Show (GDBEqn a) => {
        libId :: Int,
        libSelected :: Bool,
        libEqn :: GDBEqn a
        } -> GDBLibEntry a
deriving instance Show (GDBLibEntry a)

-- | A graph database contains mode settings, window
-- settings, and a library of functions. The latter two
-- depend on the graphcs mode.
data GDB (a :: GraphMode) where
    GDB :: (Show (GDBSettings a), Show (GDBEqn a)) => {
        gdbMode :: ModeSettings,
        gdbSettings :: GDBSettings a,
        gdbLib :: [GDBLibEntry a]
        } -> GDB a
deriving instance Show (GDB a)

-- | Variables have a type and type-specific data.
-- See also `Data.TI85.File.Variable.VarType`.
data Variable =
    TIScalar TINumber
    | TIVector [TINumber]
    | TIList [TINumber]
    | TIMatrix [[TINumber]]
    | TIConstant TINumber
    | TIEquation Text
    | TIString Text
    | TIProgram Program
    | TIPicture TIBitmap
    | TIFuncSettings FuncSettings
    | TIPolarSettings PolarSettings
    | TIParamSettings ParamSettings
    | TIDiffEqSettings DiffEqSettings
    | TIZRCL SavedWinSettings
    | TIFuncGDB (GDB Func)
    | TIPolarGDB (GDB Polar)
    | TIParamGDB (GDB Param)
    | TIDiffEqGDB (GDB DiffEq)
    deriving Show

-- * Text Conversion

-- | Utility for converting a showable
-- to Text.
showText :: Show a => a -> Text
showText = pack.show

-- | Convert a TINumber to Text.
showNumber :: TINumber -> Text
showNumber (TIReal x) = showText x
showNumber (TIComplex x y) = showText x <> "+" <> showText y <> "i"

-- | Convert a Program to Text.
showProgram :: Program -> Text
showProgram (PlainText progText) = progText
showProgram (Tokenized tokens) =
    foldMap (\(Token _ t) -> t) tokens

-- ** Window Settings

-- | Function window settings.
showFuncSettings :: FuncSettings -> Text
showFuncSettings settings =
    "\nxMin: " <> showNumber (fXMin settings)
    <> "\nxMax: " <> showNumber (fXMax settings)
    <> "\nxScl: " <> showNumber (fXScl settings)
    <> "\nyMin: " <> showNumber (fYMin settings)
    <> "\nyMax: " <> showNumber (fYMax settings)
    <> "\nyScl: " <> showNumber (fYScl settings)

-- | Polar window settings.
showPolarSettings :: PolarSettings -> Text
showPolarSettings settings =
    "\nθMin: " <> showNumber (polThetaMin settings)
    <> "\nθMax: " <> showNumber (polThetaMax settings)
    <> "\nθStep: " <> showNumber (polThetaStep settings)
    <> "\nxMin: " <> showNumber (polXMin settings)
    <> "\nxMax: " <> showNumber (polXMax settings)
    <> "\nxScl: " <> showNumber (polXScl settings)
    <> "\nyMin: " <> showNumber (polYMin settings)
    <> "\nyMax: " <> showNumber (polYMax settings)
    <> "\nyScl: " <> showNumber (polYScl settings)

-- | Parametric window settings.
showParamSettings :: ParamSettings -> Text
showParamSettings settings =
    "\ntMin: " <> showNumber (parTMin settings)
    <> "\ntMax: " <> showNumber (parTMax settings)
    <> "\ntStep: " <> showNumber (parTStep settings)
    <> "\nxMin: " <> showNumber (parXMin settings)
    <> "\nxMax: " <> showNumber (parXMax settings)
    <> "\nxScl: " <> showNumber (parXScl settings)
    <> "\nyMin: " <> showNumber (parYMin settings)
    <> "\nyMax: " <> showNumber (parYMax settings)
    <> "\nyScl: " <> showNumber (parYScl settings)

-- | DiffEq window settings.
showDiffEqSettings :: DiffEqSettings -> Text
showDiffEqSettings settings =
    "\ndiffTol: " <> showNumber (diffTol settings)
    <> "\ntPlot: " <> showNumber (diffTPlot settings)
    <> "\ntMin: " <> showNumber (diffTMin settings)
    <> "\ntMax: " <> showNumber (diffTMax settings)
    <> "\ntStep: " <> showNumber (diffTStep settings)
    <> "\nxMin: " <> showNumber (diffXMin settings)
    <> "\nxMax: " <> showNumber (diffXMax settings)
    <> "\nxScl: " <> showNumber (diffXScl settings)
    <> "\nyMin: " <> showNumber (diffYMin settings)
    <> "\nyMax: " <> showNumber (diffYMax settings)
    <> "\nyScl: " <> showNumber (diffYScl settings)
    <> "\nxAxis: " <> showAxis (diffXAxis settings)
    <> "\nyAxis: " <> showAxis (diffYAxis settings)
  where
    showAxis :: DiffEqAxis -> Text
    showAxis AxisT = "t"
    showAxis (AxisQ ai) =
        let ind = fromEnum ai
        in if ind == 0
            then "Q"
            else "Q" <> showText ind
    showAxis (AxisQ' ai) =
        let ind = fromEnum ai
        in if ind == 0
            then "Q'"
            else "Q'" <> showText ind

-- | Saved window settings.
showWinSettings :: SavedWinSettings -> Text
showWinSettings settings =
    "\nzθMin: " <> showNumber (zThetaMin settings)
    <> "\nzθMax: " <> showNumber (zThetaMax settings)
    <> "\nzθStep: " <> showNumber (zThetaStep settings)
    <> "\nztPlot: " <> showNumber (ztPlot settings)
    <> "\nztMin: " <> showNumber (ztMin settings)
    <> "\nztMax: " <> showNumber (ztMax settings)
    <> "\nztStep: " <> showNumber (ztStep settings)
    <> "\nzxMin: " <> showNumber (zxMin settings)
    <> "\nzxMax: " <> showNumber (zxMax settings)
    <> "\nzxScl: " <> showNumber (zxScl settings)
    <> "\nzyMin: " <> showNumber (zyMin settings)
    <> "\nzyMax: " <> showNumber (zyMax settings)
    <> "\nzyScl: " <> showNumber (zyScl settings)

showGDBMode :: ModeSettings -> Text
showGDBMode mode =
    "Dot/Line: " <> if modeDrawDot mode then "Dot" else "Line" <> "\n"
    <> "Simul/Seq: " <> if modeSimulG mode then "Simul" else "Seq" <> "\n"
    <> "Grid: " <> showText (modeGridOn mode) <> "\n"
    <> "Polar: " <> showText (modePolarGC mode) <> "\n"
    <> "Coord: " <> showText (not $ modeCoordOff mode) <> "\n"
    <> "Axes: " <> showText (not $ modeAxesOff mode) <> "\n"
    <> "Label: " <> showText (modeLabelOn mode) <> "\n"

showGDB :: HasGDB (a :: GraphMode) => GDB (a :: GraphMode) -> Text
showGDB gdb =
    "Mode Settings:\n"
    <> showGDBMode (gdbMode gdb) <> "\n"
    <> "Window Settings:"
    <> showGDBSettings gdb <> "\n\n"
    <> "Library:\n"
    <> showGDBHeader gdb
    <> showGDBLib gdb

-- | Convert a Variable to Text
showVariable :: Variable -> Text
showVariable (TIScalar tn) = showNumber tn
showVariable (TIVector tns) =
    let nums = map showNumber tns
    in "<" <> intercalate "," nums <> ">"
showVariable (TIList tns) =
    let nums = map showNumber tns
    in "[" <> intercalate "," nums <> "]"
showVariable (TIMatrix tnss) =
    let rows = [ intercalate "," $ map showNumber row | row <- tnss ]
    in "<<" <> intercalate "\n  " rows <> ">>"
showVariable (TIConstant tn) = showNumber tn
showVariable (TIEquation txt) = txt
showVariable (TIString txt) = txt
showVariable (TIProgram pro) = showProgram pro
showVariable (TIPicture pic) = showAsciiArt pic
showVariable (TIFuncSettings settings) = showFuncSettings settings
showVariable (TIPolarSettings settings) = showPolarSettings settings
showVariable (TIParamSettings settings) = showParamSettings settings
showVariable (TIDiffEqSettings settings) = showDiffEqSettings settings
showVariable (TIZRCL settings) = showWinSettings settings
showVariable (TIFuncGDB gdb) = showGDB gdb
showVariable (TIPolarGDB gdb) = showGDB gdb
showVariable (TIParamGDB gdb) = showGDB gdb
showVariable (TIDiffEqGDB gdb) = showGDB gdb

-- * IO

-- | Print a textual representation of a Variable.
printVariable :: Variable -> IO ()
printVariable = putStrLn.showVariable

