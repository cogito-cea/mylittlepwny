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
keyHyps :: Byte -> [Key]
keyHyps byte =
  let pre  = replicate byte 0
      post = replicate (15 - byte) 0
      keybytes = [pre ++ [k] ++ post | k <- [0..255]]
      keyw32 = map tow32 keybytes
  in  map (Key128 . RawKey) keyw32

-- | Considering the key byte 'byte' attacked (see 'keyHyps'),
--   generate the matrix of hypothetical AES init states according to
--   the input plaintexts 't'.
initStateHyps :: Byte -> Plaintext -> [State]
initStateHyps byte t =
  let keys = keyHyps byte
  in  [aesInit k t | k <- keys ]

-- | Considering the key byte 'byte' attacked (see 'keyHyps'),
--   compute the matrix of hypothetical values at the output of the
--   first SBOX, according to the input plaintexts 't'.
firstAddRK :: Byte -> Plaintext -> [Word8]
firstAddRK byte t =
  let states = initStateHyps byte t
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
  let states = initStateHyps byte t
      firstRound = (subBytes . addRoundKey) <$> states
  in  (getByte byte . toAesText) <$> firstRound

-- | Compute the hypothetic outputs of the first SBOX, for the key
-- byte 'byte', and the input plaintext 'txt'.
--
-- This is a optimised version of 'firstSBOXHyps', optimised at the
-- algorithmic level.
firstSBOXHyps' :: Byte -> Plaintext -> [Word8]
firstSBOXHyps' b = firstSBOXHypsPartial b [0..255]

-- | Compute the hypothetic outputs of the first SBOX, for the key
-- byte 'byte', the input plaintext 'txt', and a partial list of key
-- hypothetical values.
--
-- This is a optimised version of 'firstSBOXHyps', optimised at the
-- algorithmic level.
firstSBOXHypsPartial :: Byte -> [Word8] -> Plaintext -> [Word8]
firstSBOXHypsPartial byte ks txt = (subByte . xor txtbyte) <$> ks
  where
    txtbyte = (bytes txt) !! byte

-- | Power model: compute the hamming weight.
hammingWeight :: [[Word8]] -> [[Int]]
hammingWeight = (fmap.fmap) popCount

-- | Export the matrix of CPA/DPA hypothesis in a text file, using a
--   textual decimal representation.
exportHypothesis :: FilePath -> [[Int]] -> IO ()
exportHypothesis f hyp =
  writeFile f $ unlines $ map unwords $ (fmap.fmap) show hyp
