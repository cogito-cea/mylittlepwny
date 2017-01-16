module TTest
  where

import           Data.Function (on)
import           Data.List     (partition)

import           Aes

-- | Compute two streams of plaintexts for the random vs. random specific t-test.
--  @key@ is the known key value,
--  @bitnb@ the bit observed,
--  @f@ the function that computes the internal state observed in AES (e.g. 'firstSBOX').
--
--  The first stream of plaintexts is such that:
--  bit bitnt (f key  plaintext) is 0
--
--  The second stream of plaintexts is such that:
--  bit bitnt (f key  plaintext) is 1
ttestRR :: (Key -> Plaintext -> State) -> Key -> BitNumber -> IO ([Plaintext], [Plaintext])
ttestRR f key bitnb = do
  -- Export two lists of plaintexts
  -- t0, such that bit 0 of (firstSBOX t) is 0
  -- t1, such that bit 0 of (firstSBOX t) is 1
  ts <- randomPlaintexts

  -- the same key is used for all computations
  let ks = repeat key

      -- the internal state values observed, under hypothesis
      states = zipWith f ks ts
      -- keep the sbox value and its plaintext together
      couples = zip ts states
      -- split according to the bit value of the sbox output
      populations = partition (\(_, x) -> bit bitnb x == 0) couples

  return $ mapPair (map fst) populations

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = uncurry ((,) `on` f)
