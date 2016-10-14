module Main where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Word

import AesReference

-- key = Key128 $ RawKey [0x01234567, 0x89abcdef, 0x12345678, 0x9abcdef0]

-- text :: Plaintext
-- text = Plaintext [ 0x00, 0x00, 0x00, 0x00
--                  , 0x00, 0x00, 0x00, 0x00
--                  , 0x00, 0x00, 0x00, 0x00
--                  , 0x00, 0x00, 0x00, 0x00]



key :: Key
key = Key128 $ RawKey [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]

text :: Plaintext
text = Plaintext [ 0x00, 0x11, 0x22, 0x33
                 , 0x44, 0x55, 0x66, 0x77
                 , 0x88, 0x99, 0xaa, 0xbb
                 , 0xcc, 0xdd, 0xee, 0xff]

hypothesis :: Key -> Plaintext -> Ciphertext
hypothesis key plaintext =
  stateToCiphertext $
  subBytes . addRoundKey $
  setKey key $ plaintextToState plaintext

-- State -> State


main :: IO ()
main = do
  putStrLn "+++"
  print $ cipherToHex $ hypothesis key text

toHex :: Word8 -> ByteString
toHex = toLazyByteString . word8Hex

cipherToHex :: Ciphertext -> [ByteString]
cipherToHex (Ciphertext cs) = map toHex cs