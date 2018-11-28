-----------------------------------------------------------------------------
-- |
-- Module      :  Aes.Random
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Aes.Random
  ( randomPlaintexts
  , randomPlaintexts'
  ) where

import           System.Random

import           Aes.Types

-- | Compute a lazy list of random 'Plaintext' values.
--   Because of lazyness, calling this function several times will
--   produce the same list of values.  To get different lists at each
--   call, use 'randomPlaintexts'' instead.
randomPlaintexts :: IO [Plaintext]
randomPlaintexts = randoms <$> getStdGen

-- | Compute a list of 'n' random 'Plaintext' values.
--   Each call to this function produces a list of different values.
randomPlaintexts' :: Int -> IO [Plaintext]
randomPlaintexts' n = sequence $ take n $ repeat randomIO
