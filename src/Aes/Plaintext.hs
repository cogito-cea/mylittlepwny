module Aes.Plaintext
  ( randomPlaintexts
  ) where

import           System.Random

import           Aes.Types

-- | Compute a lazy list of random 'Plaintext' values.
randomPlaintexts :: IO [Plaintext]
randomPlaintexts = randoms <$> getStdGen
