{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Lazy               as BL hiding (map, putStrLn)
import           Data.ByteString.Lazy.Builder       as BL
import           Data.ByteString.Lazy.Builder.ASCII (word8Hex)
import           Data.ByteString.Lazy.Char8         as BC (unpack)
import           Data.Word

import           AesReference
import           Masking

key :: Key
key = Key128 $ RawKey [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]

text :: Plaintext
text = Plaintext [ 0x00, 0x11, 0x22, 0x33
                 , 0x44, 0x55, 0x66, 0x77
                 , 0x88, 0x99, 0xaa, 0xbb
                 , 0xcc, 0xdd, 0xee, 0xff]



main :: IO ()
main = do
  print $ aesBlockEncrypt key text
  print $ aesMaskedBlockEncrypt
    (PreSubBytesMask $ Mask8 0x13)
    (PostSubBytesMask $ Mask8 0x7A)
    (MixColumnMask 0x55 0xAA 0x69 0x8B)
    key text



hypFirstRoundSBOX :: Key -> Plaintext -> State
hypFirstRoundSBOX k t =
  subBytes . addRoundKey $
  aesInit k t

toHex :: Word8 -> ByteString
toHex = toLazyByteString . word8Hex

cipherToHex :: Ciphertext -> [ByteString]
cipherToHex (Ciphertext cs) = map toHex cs


toWord8 :: ByteString -> [Word8]
toWord8 xs =
  let space = BL.head " "
  in map (read . BC.unpack) $ BL.split space xs


{-

TODO list

pour être plus efficace

supprimer la représentation texte des plaintext.
ou plutôt prendre par défaut une représentation binaire.
- DPA scripts
- template expe
- ce truc

-}
