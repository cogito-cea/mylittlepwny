{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aes.Types
  where

import           Control.DeepSeq                    (NFData)
import           Data.Bits
import           Data.ByteString.Lazy               (ByteString)
import           Data.ByteString.Lazy.Builder       (toLazyByteString)
import           Data.ByteString.Lazy.Builder.ASCII (word8Hex)
import           Data.List
import           Data.Word


{----------------------------------------------------
   data types
 ----------------------------------------------------}

newtype Plaintext = Plaintext [Word8]
  deriving (Eq, Show, NFData)

-- | a Ciphertext is a list of Word8
newtype Ciphertext = Ciphertext [Word8]
  deriving (Eq, Show)

cipherToHex :: Ciphertext -> [ByteString]
cipherToHex (Ciphertext cs) = map (toLazyByteString . word8Hex) cs

data State = State {
                state0   :: Word32,
                state1   :: Word32,
                state2   :: Word32,
                state3   :: Word32,
                schedule :: KeySchedule }
    deriving (Eq, Show)

data Key = Key128 RawKey | Key192 RawKey | Key256 RawKey
  deriving (Show)

newtype RawKey = RawKey [Word32]
  deriving (Show)

-- TODO.
-- - je n'aime pas la définition de RawKey, parce que la taille de la
--   liste [Word32] devrait être fixe, et pouvoir prendre quelques
--   valeurs seulement, selon qu'on travaille en AES-128, AES-192 ou
--   AES-256.
-- - regarder comment ce type est implémenté dans les bibliothèques crypto.


{----------------------------------------------------
    the key schedule
 ----------------------------------------------------}
newtype KeySchedule = KeySchedule [Word32]
    deriving (Eq)
-- | we don't want to show the KeySchedule because it contains an infinite list.
instance Show KeySchedule where
  show (KeySchedule _) = "KeySchedule [*hidden*]"

defaultKeySchedule :: KeySchedule
defaultKeySchedule = KeySchedule $ repeat 0x00000000


{----------------------------------------------------
    low level operations
 ----------------------------------------------------}
octets :: Word32 -> [Word8]
octets i =
    [ fromIntegral $ i `shiftR` 24
    , fromIntegral $ i `shiftR` 16
    , fromIntegral $ i `shiftR` 8
    , fromIntegral i
    ]

fromOctets :: [Word8] -> Word32
fromOctets = foldl' shiftOp 0
    where shiftOp l b = (l `shiftL` 8) .|. (fromIntegral b)
