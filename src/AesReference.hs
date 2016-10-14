module AesReference where

import Data.Word
import Data.Bits
import Data.List
import Data.Array

{----------------------------------------------------
   data types
 ----------------------------------------------------}

newtype Plaintext = Plaintext [Word8]
  deriving (Show)

newtype Ciphertext = Ciphertext [Word8]
  deriving (Show)

data State = State {
                w0 :: Word32,
                w1 :: Word32,
                w2 :: Word32,
                w3 :: Word32,
                schedule :: KeySchedule }
    deriving (Eq, Show)

data Key = Key128 RawKey | Key192 RawKey | Key256 RawKey
  deriving (Show)

newtype RawKey = RawKey [Word32]
  deriving (Show)


{----------------------------------------------------
    operations in GF(2^8)
 ----------------------------------------------------}

gfAdd :: Bits a => a -> a -> a
gfAdd x y =
    x `xor` y

gfAdd' :: Word8 -> Word8 -> Word8
gfAdd' x y = x `xor` y

expandPolynomial :: Word32 -> [Word32]
expandPolynomial x =
        expandPoly 1 x []
    where
        expandPoly 0 _ xs = filter (/=0) xs
        expandPoly y x xs = expandPoly (y `shiftL` 1) x ((y .&. x) : xs)

polyMult :: Word32 -> Word32 -> Word32
polyMult x y =
    foldl' gfAdd 0 $ map (x*) $ expandPolynomial y

polyRemainder :: Word32 -> Word32 -> Word32
polyRemainder x y =
        if x > 255
            then polyRemainder (gfAdd x factor) y
            else x
    where
        factor = y * (greatestXFactor `div` greatestYFactor)
        greatestXFactor = head $ expandPolynomial x
        greatestYFactor = head $ expandPolynomial y

gfMult :: Word32 -> Word32 -> Word32
gfMult x y = (flip polyRemainder) 283 $ polyMult x y

multArray :: Array Word8 (Array Word8 Word8)
multArray =
    array (0, 255)
        [(fromIntegral j,
            array (0,255)
                    [(fromIntegral i, fromIntegral $ i `gfMult` j)
                    | i <- [0..255]])
            | j <- [0..255]]

gfMult' :: Word8 -> Word8 -> Word8
gfMult' x y = (multArray ! x) ! y

inverseArray :: Array Word8 Word8
inverseArray =
    array (0, 255)
          ([(0,0), (1,1)]
           ++ [(i,j) | i <- [2..255], j <- [2..255], (gfMult' i j) == 1])

divArray :: Array Word8 (Array Word8 Word8)
divArray =
    array (0, 255)
        [(j,
            array (0,255)
                    [(i, j `gfMult'` (inverseArray ! i))
                    | i <- [0..255]])
            | j <- [0..255]]
gfDiv' :: Word8 -> Word8 -> Word8
gfDiv' x y = (divArray ! x) ! y

{----------------------------------------------------
    low level operations
 ----------------------------------------------------}

octets :: Word32 -> [Word8]
octets i =
    [ fromIntegral $ i `shiftR` 24
    , fromIntegral $ i `shiftR` 16
    , fromIntegral $ i `shiftR` 8
    , fromIntegral i
    ]

fromOctets :: [Word8] -> Word32
fromOctets = foldl' shiftOp 0
    where shiftOp l b = (l `shiftL` 8) .|. (fromIntegral b)


{----------------------------------------------------
    the State Array
 ----------------------------------------------------}
plaintextToState :: Plaintext -> State
plaintextToState (Plaintext ws) =
        State firstWord secondWord thirdWord fourthWord defaultKeySchedule
    where
        makeWord xs = fromOctets $ take 4 xs
        firstWord  = makeWord ws
        secondWord = makeWord $ drop 4  ws
        thirdWord  = makeWord $ drop 8  ws
        fourthWord = makeWord $ drop 12 ws

stateToCiphertext :: State -> Ciphertext
stateToCiphertext (State w0 w1 w2 w3 _) =
        Ciphertext $ (octets w0) ++ (octets w1) ++ (octets w2) ++ (octets w3)

{----------------------------------------------------
    the key schedule
 ----------------------------------------------------}
newtype KeySchedule = KeySchedule [Word32]
    deriving (Eq, Show)

defaultKeySchedule :: KeySchedule
defaultKeySchedule = KeySchedule $ repeat 0x00000000

{----------------------------------------------------
    the AES round functions
 ----------------------------------------------------}

{----------------------------------------------------
    SubBytes
 ----------------------------------------------------}

subBytesArray :: Array Word8 Word8
subBytesArray =
        array (0, 255) [(i, affineXform $ inverseArray ! i) | i <- [0..255]]
    where
        c = 0x63
        affineXform b =
                    b
            `xor`  (b `rotateR` 4)
            `xor`  (b `rotateR` 5)
            `xor`  (b `rotateR` 6)
            `xor`  (b `rotateR` 7)
            `xor`   c

subByte :: Word8 -> Word8
subByte x = subBytesArray ! x

subWord :: Word32 -> Word32
subWord w32 =
    fromOctets $ map subByte $ octets w32

subBytes :: State -> State
subBytes st@(State w0 w1 w2 w3 _) =
        st { w0 = (subWord w0),
             w1 = (subWord w1),
             w2 = (subWord w2),
             w3 = (subWord w3) }

{----------------------------------------------------
    ShiftRows
 ----------------------------------------------------}

shiftRows :: State -> State
shiftRows st@(State w0 w1 w2 w3 _) =
        shift (octets w0) (octets w1) (octets w2) (octets w3)
    where
        shift ([w00,w01,w02,w03]) ([w10,w11,w12,w13])
              ([w20,w21,w22,w23]) ([w30,w31,w32,w33]) =
                    st { w0 = (fromOctets [w00,w11,w22,w33]),
                         w1 = (fromOctets [w10,w21,w32,w03]),
                         w2 = (fromOctets [w20,w31,w02,w13]),
                         w3 = (fromOctets [w30,w01,w12,w23]) }

{----------------------------------------------------
    MixColumns
 ----------------------------------------------------}

mixCol0 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
mixCol0 s0 s1 s2 s3 =
                (0x02 `gfMult'` s0)
        `gfAdd` (0x03 `gfMult'` s1)
        `gfAdd` (0x01 `gfMult'` s2)
        `gfAdd` (0x01 `gfMult'` s3)

mixCol1 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
mixCol1 s0 s1 s2 s3 =
                (0x01 `gfMult'` s0)
        `gfAdd` (0x02 `gfMult'` s1)
        `gfAdd` (0x03 `gfMult'` s2)
        `gfAdd` (0x01 `gfMult'` s3)

mixCol2 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
mixCol2 s0 s1 s2 s3 =
                (0x01 `gfMult'` s0)
        `gfAdd` (0x01 `gfMult'` s1)
        `gfAdd` (0x02 `gfMult'` s2)
        `gfAdd` (0x03 `gfMult'` s3)

mixCol3 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
mixCol3 s0 s1 s2 s3 =
                (0x03 `gfMult'` s0)
        `gfAdd` (0x01 `gfMult'` s1)
        `gfAdd` (0x01 `gfMult'` s2)
        `gfAdd` (0x02 `gfMult'` s3)

mixColumns :: State -> State
mixColumns st@(State w0 w1 w2 w3 _) =
        st { w0 = (mixOctets w0),
             w1 = (mixOctets w1),
             w2 = (mixOctets w2),
             w3 = (mixOctets w3) }
    where
        mixOctets w32 = mix $ octets w32
        mix ([s0,s1,s2,s3]) =
            fromOctets
                    [ mixCol0 s0 s1 s2 s3
                    , mixCol1 s0 s1 s2 s3
                    , mixCol2 s0 s1 s2 s3
                    , mixCol3 s0 s1 s2 s3]

{----------------------------------------------------
    AddRoundKey
 ----------------------------------------------------}

setKey :: Key -> State -> State
setKey key st =
    st { schedule = nistKeyExpand key }

addRoundKey :: State -> State
addRoundKey st@(State w0 w1 w2 w3 (KeySchedule schedule)) =
    st { w0 = w0 `xor` (schedule !! 0),
         w1 = w1 `xor` (schedule !! 1),
         w2 = w2 `xor` (schedule !! 2),
         w3 = w3 `xor` (schedule !! 3),
         schedule = KeySchedule $ drop 4 schedule }

{----------------------------------------------------
    key expansion
 ----------------------------------------------------}

rotWord :: Word32 -> Word32
rotWord w = rotate $ octets w
    where
        rotate [w0, w1, w2, w3] = fromOctets [w1, w2, w3, w0]

rcon :: [Word32]
rcon =
    map toWord32 $ iterate (0x02 `gfMult'`) 0x01
  where
    toWord32 w8 = fromOctets [w8, 0, 0, 0]

newtype KeyLength = KeyLength Int
    deriving (Eq, Show)
newtype Index = Index Int
    deriving (Eq, Show)
newtype KeyWord = KeyWord Word32
    deriving (Eq, Show)

keyExpand :: RawKey -> (KeyLength -> Index -> KeyWord -> Word32 -> Word32)
              -> KeySchedule
keyExpand raw@(RawKey key) subFunc =
        KeySchedule $
            key ++ (expandedKey keyLength (last key) $ keyExpand raw subFunc)
    where
        keyLength = length key
        expandedKey i temp (KeySchedule (k:ks)) =
            let newTemp =
                    (subFunc (KeyLength keyLength) (Index i) (KeyWord k) temp)
            in newTemp : expandedKey (i + 1) newTemp (KeySchedule ks)

xorWords :: Word32 -> Word32 -> Word32
xorWords = gfAdd

rotateSubWord :: KeyLength -> Index -> KeyWord -> Word32 -> Word32
rotateSubWord (KeyLength keyLength) (Index i) (KeyWord w) temp =
    flip xorWords w $
        xorWords
            (subWord (rotWord temp))
            (rcon !! ((i `div` keyLength) - 1))

subTemp :: Word32 -> Word32 -> Word32
subTemp temp w =
    xorWords (subWord temp) w

subFuncAes128 :: KeyLength -> Index -> KeyWord -> Word32 -> Word32
subFuncAes128 kl@(KeyLength keyLength) ind@(Index i) kw@(KeyWord k) temp =
    if (i `mod` keyLength) == 0
        then rotateSubWord kl ind kw temp
        else xorWords temp k

subFuncAes192 :: KeyLength -> Index -> KeyWord -> Word32 -> Word32
subFuncAes192 = subFuncAes128

subFuncAes256 :: KeyLength -> Index -> KeyWord -> Word32 -> Word32
subFuncAes256 kl@(KeyLength keyLength) ind@(Index i) kw@(KeyWord k) temp =
    if ((0 == i `mod` (keyLength `div` 2)) && (0 /= i `mod` keyLength))
        then xorWords (subWord temp) k
        else subFuncAes128 kl ind kw temp


nistKeyExpand :: Key -> KeySchedule
nistKeyExpand (Key128 raw) = keyExpand raw subFuncAes128
nistKeyExpand (Key192 raw) = keyExpand raw subFuncAes192
nistKeyExpand (Key256 raw) = keyExpand raw subFuncAes256

{----------------------------------------------------
    AES encryption
 ----------------------------------------------------}

roundsForKey :: Key -> Int
roundsForKey (Key128 _) = 10
roundsForKey (Key192 _) = 12
roundsForKey (Key256 _) = 14

aesInit :: Key -> Plaintext -> State
aesInit key plaintext = setKey key $ plaintextToState plaintext

aesBlockEncrypt :: Key -> Plaintext -> Ciphertext
aesBlockEncrypt key input =
        stateToCiphertext $
            blockCipher (roundsForKey key) $
                        addRoundKey $ aesInit key input
    where
        blockCipher rounds st =
            if rounds == 1
                then
                    addRoundKey $ shiftRows $ subBytes st
                else
                    blockCipher (rounds - 1) $
                        addRoundKey $ mixColumns $ shiftRows $ subBytes st

{----------------------------------------------------
 ----------------------------------------------------}
