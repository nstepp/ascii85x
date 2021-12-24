{-# LANGUAGE OverloadedStrings #-}
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Word

import Data.TI85

invalidID = [0x0b,0x13,0x14,0x16,0x1c]

validTypeID :: Word8 -> Bool
validTypeID val
    | val >= 0x00 && val <= 0x1e = val `notElem` invalidID
    | otherwise = False

-- | Generate a valid variable type ID
genTypeID :: Gen Word8
genTypeID = do
    Gen.filter validTypeID $ Gen.word8 (Range.linear 0x00 0x1e)


-- | Check ID mapping syncrhonization
idMapping :: Property
idMapping = property $ do
    idVal <- forAll genTypeID
    (typeToId.idToType) idVal === idVal

tests :: IO Bool
tests = checkParallel $ Group "Test.TI85" [
    ("idMapping", idMapping)
    ]

main :: IO ()
main = do
    tests
    return ()
