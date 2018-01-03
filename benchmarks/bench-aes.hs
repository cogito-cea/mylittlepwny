{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

import           Criterion.Main

import           Aes
import           Aes.Hypothesis
import           AesImport

main :: IO ()
main =
  defaultMain
  [ bgroup "encryption"
    [ bench "  nf - t0" $ nf (aesBlockEncrypt k) t0
    , bench "  nf - t1" $ nf (aesBlockEncrypt k) t1
    ]
  , bgroup "firstRoundSBOX"
    [ bench "  nf - t0 #0 " $   nf (firstSBOXHyps 0) t0
    , bench "  nf - t1 #0 " $   nf (firstSBOXHyps 0) t1
    , bench "  nf - t0 #3 " $   nf (firstSBOXHyps 3) t0
    , bench "  nf - t1 #3 " $   nf (firstSBOXHyps 3) t1
    , bench "  nf - t0 #15" $   nf (firstSBOXHyps 15) t0
    , bench "  nf - t1 #15" $   nf (firstSBOXHyps 15) t1
    ]
  , bgroup "firstSBOXHyps' [0..255]"
    [ bench "  nf - t0 #0 " $   nf (firstSBOXHyps' 0 ) t0
    , bench "  nf - t1 #0 " $   nf (firstSBOXHyps' 0 ) t1
    , bench "  nf - t0 #3 " $   nf (firstSBOXHyps' 3 ) t0
    , bench "  nf - t1 #3 " $   nf (firstSBOXHyps' 3 ) t1
    , bench "  nf - t0 #15" $   nf (firstSBOXHyps' 15) t0
    , bench "  nf - t1 #15" $   nf (firstSBOXHyps' 15) t1
    ]
  ]

t0, t1 :: Plaintext
t0 = stringImport "0x00 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88 0x99 0xaa 0xbb 0xcc 0xdd 0xee 0xff"
t1 = stringImport "0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00"

k :: Key
k = stringImport "0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f"
