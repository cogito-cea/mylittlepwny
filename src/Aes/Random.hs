module Aes.Random
  ( random
  , randoms
  , mkStdGen
  ) where

import           Data.Word
import           System.Random (mkStdGen)
import qualified System.Random as R (RandomGen, random)


import           Aes.Types

-- TODO expliquer pourquoi Plaintext ne peut pas être une instance de Random.
-- c'est une question intéressante à poser sur reddit.
-- - il faut que Plaintext soit une instance de Enum.
-- mais Enum utilise des Int comme index.  Un Int n'est pas assez grand pour classifier toutes les énumérations de Plaintext.  De toutes manières, ça n'a peut-être pas de sens d'essayer de classifier des Plaintext, puisque Plaintext fait 16 octets.
-- instance Random Plaintext where
random :: R.RandomGen g => g -> (Plaintext, g)
random g = let (ws, x) = randomW8List 16 g
               -- TODO mesures de perfs. tester avec INLINE
               randomW8List :: R.RandomGen g => Int -> g -> ([Word8], g)
               randomW8List 0 k = ([], k)
               randomW8List n k = let (x', g')  = R.random k
                                      (xs', _) = randomW8List (n-1) g'
                                  in  (xs' ++ [x'], g')
            in  (Plaintext ws, x)

randoms :: R.RandomGen g => g -> [Plaintext]
randoms g = let (x, g') = random g
            in  x : randoms g'

--  -- randomR :: (a,a) -> g -> (a, g)
--   randomR = undefined

-- instance Enum Plaintext where
--   -- toEnum :: Int -> Plaintext
--   toEnum = undefined

--   -- Problème. Int n'est pas suffisant pour représenter toute la plage de valeurs possible pour Plaintext.
--   -- fromEnum :: Plaintext -> Int
--   --
--   -- Plaintext is defined as:
--   -- Plaintext ['least significant word', ..., 'most significant word']
--   fromEnum (Plaintext ws) = foldr (\w acc -> acc*256 + (fromIntegral w)) 0 ws
