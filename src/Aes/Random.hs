module Aes.Random
  where

import           Data.Word
import qualified System.Random as R

import           Aes.Types

{- |
Define 'Plaintext' as an instance of 'Random'.

The current implementation does not considers 'Plaintext' as a
sequentially ordered type.  For more information read the doc info of
'randomR'
-}

instance R.Random Plaintext where
  random = random
  randomR = randomR

random :: R.RandomGen g => g -> (Plaintext, g)
random g = let (ws, x) = randomW8List 16 g
               -- TODO mesures de perfs. tester avec INLINE
               randomW8List :: R.RandomGen g => Int -> g -> ([Word8], g)
               randomW8List 0 k = ([], k)
               randomW8List n k = let (x', g')  = R.random k
                                      (xs', _) = randomW8List (n-1) g'
                                  in  (xs' ++ [x'], g')
            in  (Plaintext ws, x)

-- | 'randomR' considers each byte value separately:
-- considering a = Plaintext [lo0, lo1, ..., lo15]
--             b = Plaintext [hi0, hi1, ..., hi15]
-- we return a random plaintext x = Plaintext [x0, x1, ..., x15] such that
-- lo0 <= x0 <= hi0
-- lo1 <= x1 <= hi1
-- ...
-- lo15 <= x15 <= hi15
--
-- 'Plaintext' cannot be made an instance of 'Integral' as instances
-- of 'Integral' also need to be instances of 'Real' and 'Enum'.  To
-- treat 'Plaintext' as a sequentially ordered type, we would need ad
-- hoc implementations of 'toInteger' and 'fromInteger'.  Considering
-- the large 'Integer' values that would be manipulated, the resulting
-- implementation of 'randomR' is expected to be slower.
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
