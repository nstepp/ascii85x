
-- | Specifics of the TI-85 variables themselves
-- (i.e. not their representation in the file).
module Data.TI85.Var where

import Prelude hiding (concat, putStrLn)
import Data.Text (Text, concat, pack, intercalate)
import Data.Text.IO (putStrLn)
import Data.TI85.Token (TokenDef)

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

-- * IO

-- | Print a textual representation of a Variable.
printVariable :: Variable -> IO ()
printVariable = putStrLn.showVariable

