{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Lazy       as BL hiding (map, putStrLn)
import           Data.ByteString.Lazy.Char8 as BC (unpack)
import           Data.Word

import           Aes.Hypothesis
import           AesImport
import           AesReference
import           Masking

thekey :: Key
thekey = stringImport "1 35 69 103 137 171 205 239 18 52 86 120 154 188 222 240"

thetext :: Plaintext
thetext = stringImport "0 17 34 51 68 85 102 119 136 153 170 187 204 221 238 255"

main :: IO ()
main = do
  print $ aesBlockEncrypt thekey thetext
  print $ aesMaskedBlockEncrypt
    (PreSubBytesMask $ Mask8 0x13)
    (PostSubBytesMask $ Mask8 0x7A)
    (MixColumnMask 0x55 0xAA 0x69 0x8B)
    thekey thetext
  texts <- importTexts "plaintexts.txt"
  let hyps = hammingWeight $ firstRoundSBOX 0 texts
  exportHypothesis "test.txt" hyps

toWord8 :: ByteString -> [Word8]
toWord8 xs =
  let space = BL.head " "
  in map (read . BC.unpack) $ BL.split space xs
