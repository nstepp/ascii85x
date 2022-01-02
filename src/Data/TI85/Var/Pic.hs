module Data.TI85.Var.Pic (
    TIBitmap,
    emptyBitmap,
    fromBytes,
    toBytes,
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

newtype TIBitmap = TIBitmap ByteString deriving Show

_bmCols = 128
_bmRows = 63
_bmSizeBytes = 1008

emptyBitmap :: TIBitmap
emptyBitmap = TIBitmap (BS.replicate _bmSizeBytes 0x0)

fromBytes :: ByteString -> Maybe TIBitmap
fromBytes bytes = if BS.length bytes == 1008
    then Just (TIBitmap bytes)
    else Nothing

toBytes :: TIBitmap -> ByteString
toBytes (TIBitmap bytes) = bytes

explodeWord :: Word8 -> ByteString
explodeWord w =
    let (bits,_) = BS.unfoldrN 8 (\x -> Just (x .&. 1, shiftR x 1)) w
    in BS.reverse bits

bytesToPixels :: ByteString -> Vector Pixel8
bytesToPixels =  fromList . map (*255) . BS.unpack . BS.concatMap explodeWord

bytesToImage :: ByteString -> Int -> Int -> DynamicImage
bytesToImage bytes width height =
    let pixels = bytesToPixels bytes
    in ImageY8 (Image width height pixels)

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

writePicPng :: FilePath -> TIBitmap -> IO ()
writePicPng path (TIBitmap bitmap) =
    let img = bytesToImage bitmap _bmCols _bmRows
    in savePngImage path img

