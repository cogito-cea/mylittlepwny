{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aes.Types
  where

import           Data.Bits
import           Data.ByteString.Lazy               (ByteString)
import           Data.ByteString.Lazy.Builder       (toLazyByteString)
import           Data.ByteString.Lazy.Builder.ASCII (word8Hex)
import           Data.List
import           Data.Word
import qualified System.Random as R

{----------------------------------------------------
   data types
 ----------------------------------------------------}

newtype Plaintext = Plaintext [Word8]
  deriving (Eq, Show)

-- |
-- Define 'Plaintext' as an instance of 'Random'.
-- 
-- The current implementation does not considers 'Plaintext' as a
-- sequentially ordered type.  For more information read the doc info of
-- 'randomR'
instance R.Random Plaintext where
  random = random
  randomR = randomR

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
    instances of the Random typeclass
 ----------------------------------------------------}
random :: R.RandomGen g => g -> (Plaintext, g)
random g = let (g0, g1) = R.split g
            in (Plaintext $ take 16 $ R.randoms g0, g1)

-- | 'randomR' considers each byte value separately:
-- considering a = Plaintext [lo0, lo1, ..., lo15]
--             b = Plaintext [hi0, hi1, ..., hi15]
-- we return a random plaintext x = Plaintext [x0, x1, ..., x15] such that
-- lo0 <= x0 <= hi0
-- lo1 <= x1 <= hi1
-- ...
-- lo15 <= x15 <= hi15
--
-- 'Plaintext' cannot easily be made an instance of 'Integral' as
-- instances of 'Integral' also need to be instances of 'Real' and
-- 'Enum'.  To treat 'Plaintext' as a sequentially ordered type, we
-- would need ad hoc implementations of 'toInteger' and 'fromInteger'.
-- Considering the large 'Integer' values that would be manipulated,
-- the resulting implementation of 'randomR' is expected to be slower.
randomR :: R.RandomGen g => (Plaintext, Plaintext) -> g -> (Plaintext, g)
randomR (Plaintext los, Plaintext his) g =
  let
    -- build a list of (lo, hi) values
    ws = zip los his
    -- we will 'scan' over 'ws' in order to apply 'randomR' to each
    -- tuple in 'ws', and forward the new generator 'g' to the next
    -- application of 'randomR'.
    -- in order to apply 'randomR', the 'step' function would have type
    -- step :: g -> (a, a) -> (a, g)
    -- which can be rewritten with type:
    -- step' :: (a, g) -> (a, a) -> (a, g)
    step :: R.RandomGen g => (Word8, g) -> (Word8, Word8) -> (Word8, g)
    step (_, h) lohi = R.randomR lohi h
    ws' = scanl step (0, g) ws
    -- the resulting generator
    g' = snd $ last ws'
    -- build the plaintext
    plaintext = Plaintext $ map fst ws'
  in  (plaintext, g')

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
