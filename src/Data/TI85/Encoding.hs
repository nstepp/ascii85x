{-# LANGUAGE  OverloadedStrings #-}

-- | Text data in a variable file is encoded using a
-- special purpose code page. This module supports
-- decoding such text, using an approximate projection
-- into unicode.
module Data.TI85.Encoding (
    tiDecode
    ) where


import Data.Word
import Data.Array (Array, listArray, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

-- | An approximation of the TI-86 code page as unocide.
tiCodePage :: Array Word8 Text
tiCodePage = listArray (0x00,0xff) [
    "■","b","o","d","h","▸","⬆","⬇","∫","⨉","A","B","C","D","E","F",
    "√","⁻¹","²","∠","°","ʳ","ᵀ","≤","≠","≥","⁻","ᴇ","→","₁₀","↑","↓",
    " ","!","\"","#","$","%","&","'","(",")","*","+",",","-",".","/",
    "0","1","2","3","4","5","6","7","8","9",":",";","<","=",">","?",
    "@","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O",
    "P","Q","R","S","T","U","V","W","X","Y","Z","[","\\","]","^","_",
    "`","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o",
    "p","q","r","s","t","u","v","w","x","y","z","{","|","}","~","=",
    "₀","₁","₂","₃","₄","₅","₆","₇","₈","₉","Á","À","Â","Ä","á","à",
    "â","ä","É","È","Ê","Ë","é","è","ê","ë","Í","Ì","Î","Ï","í","ì",
    "î","ï","Ó","Ò","Ô","Ö","ó","ò","ô","ö","Ú","Ù","Û","Ü","ú","ù",
    "û","ü","Ç","ç","Ñ","ñ","´","`","¨","¿","¡","α","β","γ","Δ","δ",
    "ε","θ","λ","μ","π","ρ","Σ","σ","τ","φ","Ω","x̅","y̅","ˣ","…","◂",
    "■","≀","-","²","°","³","\n","➡","╲","╲","◥","◣","⊸","∘","⋱","█",
    "⇧","A","a","_","↥","A̲","a̲","▩","▫","₊",".","⁴","=","■","■","■",
    "■","■","■","■","■","■","■","■","■","■","■","■","■","■","■","■"]


-- |Decode a bytestring into text using the 
-- TI-86 screen code page.
tiDecode :: ByteString -> Text
tiDecode = T.concat . map (tiCodePage!) . BS.unpack

