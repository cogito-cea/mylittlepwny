{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Aes.Bytes
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Aes.Bytes
  ( HasByte
  , bytes
  )
  where

import           Data.Word (Word8)

import           Aes.Types

class HasByte a where
  bytes :: a -> [Word8]

instance HasByte Plaintext where
  bytes (Plaintext txt) = txt

instance HasByte Key where
  bytes (Key128 (RawKey ks)) = concat $ map octets' ks
  bytes (Key192 (RawKey ks)) = concat $ map octets' ks
  bytes (Key256 (RawKey ks)) = concat $ map octets' ks
