module Masking where

import           Data.Array
import           Data.Bits
import           Data.Word

import           Aes.Reference
import           Aes.Types
import           Aes.Util


-- TODO
-- faire la séparation entre le schéma de masquage en général, et
-- l'implem Herbst 2006

class Mask a where
  toMask :: a -> StateMask

data StateMask = StateMask
  { mask0 :: Mask32
  , mask1 :: Mask32
  , mask2 :: Mask32
  , mask3 :: Mask32
  } deriving (Eq, Show)

newtype Mask8  = Mask8 Word8
  deriving (Show)

instance Mask Mask8 where
  toMask (Mask8 w) =
    let w' = fromIntegral w :: Word32
        m =   (w' `shiftL` 24)
          .|. (w' `shiftL` 16)
          .|. (w' `shiftL` 8)
          .|. (w')
    in StateMask m m m m

type Mask32 = Word32
newtype PreSubBytesMask = PreSubBytesMask Mask8
  deriving (Show)
newtype PostSubBytesMask = PostSubBytesMask Mask8
  deriving (Show)

data MixColumnMask =
  MixColumnMask
  { mc0 :: Word8
  , mc1 :: Word8
  , mc2 :: Word8
  , mc3 :: Word8
  } deriving (Show)

instance Mask MixColumnMask where
  toMask (MixColumnMask m0 m1 m2 m3) =
    let m0' = fromIntegral m0 :: Word32
        m1' = fromIntegral m1 :: Word32
        m2' = fromIntegral m2 :: Word32
        m3' = fromIntegral m3 :: Word32
        m =   (m0' `shiftL` 24)
          .|. (m1' `shiftL` 16)
          .|. (m2' `shiftL` 8)
          .|. (m3')
    in StateMask m m m m


mask :: StateMask -> State -> State
mask (StateMask m0 m1 m2 m3) (State w0 w1 w2 w3 ks) =
  State (m0 `xor` w0)
        (m1 `xor` w1)
        (m2 `xor` w2)
        (m3 `xor` w3)
        ks

-- | compute a new mask resulting from the application of the mixColumns function.
mixColumnsMask :: StateMask -> StateMask
mixColumnsMask (StateMask w0 w1 w2 w3) =
  StateMask (mixOctets w0)
            (mixOctets w1)
            (mixOctets w2)
            (mixOctets w3)
  where
    mixOctets w32 = mix $ octets w32
    mix ([s0,s1,s2,s3]) =
      fromOctets [ mixCol0 s0 s1 s2 s3
                 , mixCol1 s0 s1 s2 s3
                 , mixCol2 s0 s1 s2 s3
                 , mixCol3 s0 s1 s2 s3
                 ]

-- | pre-compute the masked SBOX lookup table.  Given two masks @m@
--   and @m'@, compute a new Sbox @S'@ such that
--   @S'(s `xor` m) = S(s) `xor` m'@.
maskedSubBytes :: Mask8 -> Mask8 -> State -> State
maskedSubBytes m m' = \st ->
  State (subWord $ state0 st)
        (subWord $ state1 st)
        (subWord $ state2 st)
        (subWord $ state3 st)
        (schedule st)
  where
    maskedSubByte i = maskedSubBytesArray m m' ! i
    subWord w32 =
        fromOctets $ map maskedSubByte $ octets w32

-- | the new subBytes array, given two input mask bytes @m@ and @m'@
maskedSubBytesArray :: Mask8 -> Mask8 -> Array Word8 Word8
maskedSubBytesArray (Mask8 m) (Mask8 m') =
  array (0, 255) [(i `xor` m, m' `xor` subByte i) | i <- [0..255]]

aesMaskedBlockEncrypt :: PreSubBytesMask  -- ^ the mask at the input of the SBOX
                      -> PostSubBytesMask -- ^ the mask at the ouput of the SBOX
                      -> MixColumnMask    -- ^ the four mask bytes for MixColumn
                      -> Key
                      -> Plaintext
                      -> Ciphertext
aesMaskedBlockEncrypt (PreSubBytesMask pre) (PostSubBytesMask post) mcmask key input =
  -- stateToCiphertext $
  --     blockCipher (roundsForKey key) $
  --                 addRoundKey $ aesInit key input
  stateToCiphertext $ encrypt $ aesInit key input
    where
        -- the pre- and post- SubBytes masks
        m  = toMask pre
        m' = toMask post

        -- the MixColumn mask,
        -- M = (m1, m2, m3, m4)
        mc    = toMask mcmask
        -- the mask resulting from the application of MixColums,
        -- M' = (m'1, m'2, m'3, m'4)
        mc'   = mixColumnsMask mc

        -- the masked sbox
        subBytes' = maskedSubBytes pre post

        encrypt = mask mc'
                ° mask m      -- TODO à supprimer après modif ks
                ° addRoundKey
                ° mask mc'
                ° blockCipher (roundsForKey key)
                ° mask m'
                ° mask m
        blockCipher rounds =
            if rounds == 1
            then subBytes'
                 ° shiftRows
                 ° mask m      -- TODO à supprimer après modif ks
                 ° addRoundKey -- key schedule is masked with m
            else subBytes'
                 ° shiftRows
                 ° mask mc
                 ° mask m'
                 ° mixColumns
                 ° mask m      -- TODO à supprimer après modif ks
                 ° addRoundKey
                 ° mask mc'
                 ° blockCipher (rounds - 1)
  -- TODO annuler le masque m dans le key schedule
  -- TODO appliquer le masque m dans le key schedule

-- blockCipher rounds =
--     if rounds == 1
--         then subBytes ° shiftRows ° addRoundKey -- key schedule is masked with m
--         else (blockCipher (rounds - 1)) ° subBytes
