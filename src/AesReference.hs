module AesReference
  ( -- data definitions
    Plaintext
  , Key
  , State
  , Ciphertext
  , RawKey
    -- functions
  , aesBlockEncrypt
  , aesInit
  , subBytes
  , addRoundKey
  , ) where

import           Aes.Reference
import           Aes.Types
