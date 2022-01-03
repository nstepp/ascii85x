-- | Picture variable types and utilities.
module Data.TI85.Var.Pic (
    -- * TI Bitmap
    TIBitmap,
    emptyBitmap,
    fromBytes,
    toBytes,
    -- * Display utilities
    showAsciiArt,
    writePicPng
    ) where

import Prelude hiding (take, drop)
import Data.Text (Text, pack, intercalate, take, drop)
import Data.Text.Encoding (decodeLatin1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Codec.Picture
import Data.Bits
import Data.Word
import Data.Vector.Storable hiding (map,take,drop)
import Numeric (showHex)

import Data.TI85.Encoding (tiDecode)

-- | A TI Picture variable is encoded as a packed bitmap,
-- and is always 128x63 binary pixels.
newtype TIBitmap = TIBitmap ByteString deriving Show

_bmCols = 128
_bmRows = 63
_bmSizeBytes = 1008

-- | A blank (all zero) bitmap
emptyBitmap :: TIBitmap
emptyBitmap = TIBitmap (BS.replicate _bmSizeBytes 0x0)

-- | Create a bitmap from a packed `ByteString`. If the data
-- is the wrong size, returns `Nothing`.
fromBytes :: ByteString -> Maybe TIBitmap
fromBytes bytes = if BS.length bytes == 1008
    then Just (TIBitmap bytes)
    else Nothing

-- | Extract the raw `ByteString` from a bitmap.
toBytes :: TIBitmap -> ByteString
toBytes (TIBitmap bytes) = bytes

-- Expands an 8-bit word into an 8-element ByteString
explodeWord :: Word8 -> ByteString
explodeWord w =
    let (bits,_) = BS.unfoldrN 8 (\x -> Just (x .&. 1, shiftR x 1)) w
    in BS.reverse bits

-- Very approximate calculator-like colors
screenColorMap :: Word8 -> Pixel8
screenColorMap 0x0 = 0xc6
screenColorMap _ = 0x39

-- Convert a `ByteString`, where each bit represents a pixel into
-- a vector of pixels suitable for use in a `DynamicImage`.
-- Bits are mapped to pixels using a vaguely calculator-like
-- color map (light gray background, dark gray foreground).
bytesToPixels :: ByteString -> Vector Pixel8
bytesToPixels =  fromList . map screenColorMap . BS.unpack . BS.concatMap explodeWord

-- Create an image out of a packed `ByteString`.
-- See `bytesToPixels`
bytesToImage :: ByteString -> Int -> Int -> DynamicImage
bytesToImage bytes width height =
    let pixels = bytesToPixels bytes
    in ImageY8 (Image width height pixels)

-- | Create a text-based representation of a picture.
-- Background pixels are rendered as spaces, while
-- foreground are rendered as full block glyphs (█).
showAsciiArt :: TIBitmap -> Text
showAsciiArt (TIBitmap bytes) =
    let bits = BS.concatMap explodeWord bytes
        --hexChars = pack $ BS.foldr' showHex "" bits
        --imgChars = chunk _bmCols hexChars []
        glyphs = tiDecode $ BS.map toGlyph bits
        imgChars = chunk _bmCols glyphs []
    in intercalate "\n" imgChars
  where
    toGlyph :: Word8 -> Word8
    toGlyph 0x0 = 0x20
    toGlyph _ = 0xdf

    chunk :: Int -> Text -> [Text] -> [Text]
    chunk n "" chunks = chunks
    chunk n xs chunks = take n xs : chunk n (drop n xs) chunks

-- | Save a bitmap as a PNG image.
-- Bits are mapped to pixels using a vaguely calculator-like
-- color map (light gray background, dark gray foreground).
writePicPng :: FilePath -> TIBitmap -> IO ()
writePicPng path (TIBitmap bitmap) =
    let img = bytesToImage bitmap _bmCols _bmRows
    in savePngImage path img

