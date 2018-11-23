module Aes.Hypothesis
  where

import           Data.Bits     (popCount, xor)
import           Data.Word     (Word8)

import           Aes
import           Aes.Reference (subByte)
import           Aes.Types
import           Aes.Util
import           AesImport

-- | Generate a full set of (256) AES key hypothesis on byte number 'Byte'.
--   All the other byte values are set to zero.
--
--   'byte' is a 'Byte', i.e. an 'Int' value expected between 0 and
--   15, 23, 31 included for AES-128, AES-196 and AES-256 respectively.
keyHypothesis :: Byte -> [Key]
keyHypothesis byte =
  let pre  = replicate byte 0
      post = replicate (15 - byte) 0
      keybytes = [pre ++ [k] ++ post | k <- [0..255]]
      keyw32 = map tow32 keybytes
  in  map (Key128 . RawKey) keyw32

-- | Considering the key byte 'byte' attacked (see 'keyHypothesis'),
--   generate the matrix of hypothetical AES init states according to
--   the input plaintexts 't'.
initState :: Byte -> Plaintext -> [State]
initState byte t =
  let keys = keyHypothesis byte
  in  [aesInit k t | k <- keys ]

-- | Considering the key byte 'byte' attacked (see 'keyHypothesis'),
--   compute the matrix of hypothetical values at the output of the
--   first SBOX, according to the input list of plaintexts 'ts'.
firstAddRK :: Byte -> Plaintext -> [Word8]
firstAddRK byte t =
  let states = initState byte t
      firstRound = addRoundKey <$> states
  in  (getByte byte . toAesText) <$> firstRound

-- | Compute the output of the first SBOX
firstSBOX :: Key -> Plaintext -> State
firstSBOX k t = aesInit k t $$ addRoundKey $$ subBytes

-- | Compute the hypothetic outputs of the first SBOX, for the key
--   byte 'byte' attacked (see 'keyHyps'), and for the input plaintext
--   't'.
firstSBOXHyps :: Byte -> Plaintext -> [Word8]
firstSBOXHyps byte t =
  let states = initState byte t
      firstRound = (subBytes . addRoundKey) <$> states
  in  (getByte byte . toAesText) <$> firstRound

-- | Compute the hypothetic outputs of the first SBOX, for the key
-- byte 'byte', and the input plaintext 'txt'.
--
-- This function provides the same results as 'firstSBOXHyps', but the
-- implementation of this function is optimised at the algorithmic
-- level.
firstSBOXHyps' :: Byte -> Plaintext -> [Word8]
firstSBOXHyps' b = firstSBOXHypsPartial b [0..255]

-- | Compute the hypothetic outputs of the first SBOX, for the key
-- byte 'byte', the input plaintext 'txt', and a partial list of key
-- hypothetical values.
firstSBOXHypsPartial :: Byte -> [Word8] -> Plaintext -> [Word8]
firstSBOXHypsPartial byte ks txt = (subByte . xor txtbyte) <$> ks
  where
    txtbyte = (bytes txt) !! byte

fstSBOX' :: Byte -> Word8 -> [Plaintext] -> [Word8]
fstSBOX' byte k txts = [subByte $ xor b k | b <- txtbyte]
  where
    txtbyte = [(bytes txt) !! byte | txt <- txts]
{-# INLINABLE fstSBOX' #-}

fstSBOX'' :: Byte -> Word8 -> Plaintext -> Word8
fstSBOX'' byte k txt = subByte $ xor txtbyte k
  where
    txtbyte = (bytes txt) !! byte
{-# INLINABLE fstSBOX'' #-}

-- | Power model: compute the hamming weight.
hammingWeight :: [[Word8]] -> [[Int]]
hammingWeight = (fmap.fmap) popCount
{-# INLINABLE hammingWeight #-}

-- | Export the matrix of CPA/DPA hypothesis in a text file, using a
--   textual decimal representation.
exportHypothesis :: FilePath -> [[Int]] -> IO ()
exportHypothesis f hyp =
  writeFile f $ unlines $ map unwords $ (fmap.fmap) show hyp
