module Aes.Hypothesis
  where

import           Data.Bits    (popCount)
import           Data.Word    (Word8)

import           Aes.Types
import           AesImport
import           AesReference

-- | Generate a set of key hypothesis for attacking AES-128 with
--  CPA/DPA on byte 'Byte'.
keyHypothesis :: Byte -> [Key]
keyHypothesis byte =
  let pre = replicate byte 0
      post = replicate (15 - byte) 0
      keybytes = [pre ++ [k] ++ post | k <- [0..255]]
      keyw32 = map tow32 keybytes
  in  map (Key128 . RawKey) keyw32

initState :: Byte -> [Plaintext] -> [[State]]
initState byte ts =
  let keys = keyHypothesis byte
  in  [fmap (`aesInit` t) keys | t <- ts]

firstRoundSBOX :: Byte -> [Plaintext] -> [[Word8]]
firstRoundSBOX byte ts =
  let states = initState byte ts
      firstRound = (fmap . fmap) (subBytes . addRoundKey) states
  in  (fmap.fmap) (getByte byte . toAesText) firstRound

hammingWeight :: [[Word8]] -> [[Int]]
hammingWeight = (fmap.fmap) popCount
