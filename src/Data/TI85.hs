{-|
Module      : Data.TI85
Description : TI-85 variable file utilities.
Copyright   : (c) Nigel Stepp, 2021
License     : GPL-3
Maintainer  : stepp@atistar.net
Stability   : experimental
Portability : POSIX

This module is meant to display information from
TI-85 variable files in a human readable way.

There were a few reasons for making this in 2021:

1. ascii85p, an ancient program for displaying
   program text, does not come with source code
2. TokenIDE, a more modern and very featureful
   tool, is written in C#, and I didn't want to
   use mono.
3. TokenIDE also does not come with source code.
4. Just let me have some fun.

-}
module Data.TI85 (
    module Data.TI85.Encoding,
    module Data.TI85.Token,
    module Data.TI85.Parsers,
    module Data.TI85.Var,
    module Data.TI85.VarFile,
    version
    ) where

import Data.TI85.Encoding
import Data.TI85.Token
import Data.TI85.Parsers
import Data.TI85.Var
import Data.TI85.VarFile

import Data.Text (Text, pack)

version :: Text
version = pack "0.1.2.0"

