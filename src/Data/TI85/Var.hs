
-- | Specifics of the TI-85 variables themselves
-- (i.e. not their representation in the file).
module Data.TI85.Var where

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
    | TIZRCL SavedWinSettings
    deriving Show

-- * Text Conversion

-- | Utility for converting a showable
-- to Text.
showText :: Show a => a -> Text
showText = pack.show

-- | Convert a TINumber to Text
showNumber :: TINumber -> Text
showNumber (TIReal x) = showText x
showNumber (TIComplex x y) = showText x <> "+" <> showText y <> "i"

-- | Convert a Program to Text
showProgram :: Program -> Text
showProgram (PlainText progText) = progText
showProgram (Tokenized tokens) =
    foldMap (\(Token _ t) -> t) tokens

showWinSettings :: SavedWinSettings -> Text
showWinSettings settings =
    "\nzThetaMin: " <> showNumber (zThetaMin settings)
    <> "\nzThetaMax: " <> showNumber (zThetaMax settings)
    <> "\nzThetaStep: " <> showNumber (zThetaStep settings)
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
showVariable (TIZRCL settings) = showWinSettings settings

-- * IO

-- | Print a textual representation of a Variable.
printVariable :: Variable -> IO ()
printVariable = putStrLn.showVariable

