-----------------------------------------------------------------------------
-- |
-- Module      :  AES
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of AES in Haskell, for educational purposes.
-----------------------------------------------------------------------------


module Aes
  ( -- * data definitions
    Plaintext
  , Key
  , State
  , Ciphertext
  , RawKey

  -- * AES round functions
  , aesBlockEncrypt
  , aesInit
  , subBytes
  , addRoundKey

  -- * module Aes.Random
  , randomPlaintexts
  , randomPlaintexts'

  -- * module Aes.Bits
  , bit
  , Bit
  , BitPos
  , HasBitPos

  -- * module Aes.Bytes
  , bytes
  ) where

import           Aes.Bits
import           Aes.Bytes
import           Aes.Random
import           Aes.Reference
import           Aes.Types
