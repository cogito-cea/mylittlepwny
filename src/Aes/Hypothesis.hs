module Aes.Hypothesis
  where

import           Data.Bits (popCount)
import           Data.Word (Word8)

import           Aes
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
  let pre = replicate byte 0
      post = replicate (15 - byte) 0
      keybytes = [pre ++ [k] ++ post | k <- [0..255]]
      keyw32 = map tow32 keybytes
  in  map (Key128 . RawKey) keyw32

-- | Considering the key byte 'byte' attacked (see 'keyHypothesis'),
--   generate the matrix of hypothetical AES init states according to
--   the input list of plaintexts 'ts'.
initState :: Byte -> [Plaintext] -> [[State]]
initState byte ts =
  let keys = keyHypothesis byte
  in  [fmap (flip aesInit t) keys | t <- ts]

-- | Compute the output of the first SBOX
firstSBOX :: Key -> Plaintext -> State
firstSBOX k t = aesInit k t $$ addRoundKey $$ subBytes

-- | Considering the key byte 'byte' attacked (see 'keyHypothesis'),
--   compute the matrix of hypothetical values at the output of the
--   first SBOX, according to the input list of plaintexts 'ts'.
firstRoundSBOX :: Byte -> [Plaintext] -> [[Word8]]
firstRoundSBOX byte ts =
  let states = initState byte ts
      firstRound = (fmap . fmap) (subBytes . addRoundKey) states
  in  (fmap.fmap) (getByte byte . toAesText) firstRound

-- | Power model: compute the hamming weight.
hammingWeight :: [[Word8]] -> [[Int]]
hammingWeight = (fmap.fmap) popCount

-- | Export the matrix of CPA/DPA hypothesis in a text file, using a
--   textual decimal representation.
exportHypothesis :: FilePath -> [[Int]] -> IO ()
exportHypothesis f hyp =
  writeFile f $ unlines $ map unwords $ (fmap.fmap) show hyp
