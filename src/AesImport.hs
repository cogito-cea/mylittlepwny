{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AesImport
  ( AesText
  , Byte
  , getByte
  , tow32

  , HasAesText(..)

  , exportTexts
  , importTexts

  , toHex
  ) where

import           Data.List (foldl')
import           Data.Word
import Text.Printf (printf)

import           Aes.Types

-- | A generic data structure to handle import and export.  The data
--   structure describes is a fixed-length vector of words.
data AesText = AesText
  Int     --   the size of the plaintext, in number of bytes
  [Word8] --   the plaintext bytes; the least significant byte is
          --   stored first in the list.
  deriving (Show)

type Byte = Int

getByte :: Byte -> AesText -> Word8
getByte x (AesText _ t) = t !! x

instance Monoid AesText where
  mempty = AesText 0 []
  mappend (AesText n t) (AesText n' t') =
    AesText (n+n') (t ++ t')
  mconcat = foldr mappend mempty

class HasAesText a where
  toAesText :: a -> AesText
  fromAesText :: AesText -> a
  -- | Import from Strings. A decimal integer representation is expected.
  -- >>> stringImport  "00"  :: String
  -- "0"
  -- >>> stringImport  ""  :: String
  -- ""
  -- >>> stringImport  "0"  :: Key
  -- *** Exception: unknown key length: 1
  -- ...
  -- >>> stringImport  "00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15"  :: Key
  -- Key128 (RawKey [66051,67438087,134810123,202182159])
  -- >>> stringImport  "00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23"  :: Key
  -- Key192 (RawKey [66051,67438087,134810123,202182159,269554195,336926231])
  -- >>> stringImport  "00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31"  :: Key
  -- Key256 (RawKey [66051,67438087,134810123,202182159,269554195,336926231,404298267,471670303])
  --
  stringImport :: String -> a
  stringImport = fromAesText . toAesText
  -- | Export to Strings, using a decimal integer textual representation.
  exportString :: a -> String
  exportString = fromAesText . toAesText

instance HasAesText String where
  -- | create a 'AesText' from a textual string representation.
  --   Expected string format: decimal integers separated by space characters.
  --
  --   The least significant byte is expected to come first in the list of input string values.
  toAesText xs = foldl' step mempty (words xs)
    where
      step :: AesText -> String ->  AesText
      step x t = mappend x (AesText 1 [read t])
  -- | Create a textual 'String' representation from a 'AesText'.
  fromAesText (AesText _ ts) = unwords $ map show ts

instance HasAesText Key where
  toAesText (Key128 (RawKey bytes)) = AesText 16 (concat $ map octets' bytes)
  toAesText (Key192 (RawKey bytes)) = AesText 24 (concat $ map octets' bytes)
  toAesText (Key256 (RawKey bytes)) = AesText 32 (concat $ map octets' bytes)

  -- | create an AES 'Key'.  Calls 'error' if the input AesText is not correctly sized.
  -- TODO: key :: AesText -> Maybe Key
  fromAesText (AesText 16 bs) = Key128 $ RawKey $ tow32 $ bs
  fromAesText (AesText 24 bs) = Key192 $ RawKey $ tow32 $ bs
  fromAesText (AesText 32 bs) = Key256 $ RawKey $ tow32 $ bs
  fromAesText (AesText n _)   = error $ "unknown key length: " ++ show n

instance HasAesText Plaintext where
  toAesText (Plaintext ts) = AesText (length ts) ts
  fromAesText (AesText _ ts) = Plaintext ts

instance HasAesText Ciphertext where
  toAesText (Ciphertext ts) = AesText (length ts) ts
  fromAesText (AesText _ ts) = Ciphertext ts

instance HasAesText State where
  -- | Convert a State to AesText.  The conversion drops the KeySchedule part of State.
  toAesText (State s0 s1 s2 s3 _) =
    AesText 16 $ octets' s0 ++ octets' s1 ++ octets' s2 ++ octets' s3

  -- | a 'State' cannot be imported from text because we cannot define
  -- the 'KeySchedule' data without knowledge of the 'Key' value.  calling
  -- 'fromAesText' calls 'error'.
  --
  -- To avoid this, one would need to define two separate typeclasses
  -- for import and export.  'State' would only be an instance of the
  -- export class.
  fromAesText = error "Cannot import a State value.  The keySchedule function is unknown."

-- | file import, with Strings.
importTexts :: HasAesText a =>
               FilePath   -- ^ the filename of the input plaintexts file
            -> IO [a]
importTexts f = do
  raw <- lines <$> Prelude.readFile f
  return $ map stringImport raw

-- | file export.
exportTexts :: HasAesText a =>
               FilePath   -- ^ the filename of the output file
            -> [a]
            -> IO ()
exportTexts f ts = Prelude.writeFile f $ unlines $ map exportString ts

-- | Translate to the same hexadecimal representation than the one
--   used in the FIPS 197 note, section C.1
toHex :: AesText -> String
toHex (AesText _ xs) = foldl step "" xs
  where
    step string x = string ++ printf "%02x" x

-- | Convert each group of 4 'Word8's to a 'Word32', following a Big Endian-like representation.
tow32 :: [Word8] -> [Word32]
tow32 [] = []
tow32 ws = let (w32, rest) = splitAt 4 ws
  in fromOctets w32 : tow32 rest

--
-- | file import, with BL.ByteString
--
-- readFile' :: String         -- ^ the filename of the input plaintexts file
--          -> IO [PlainText]
-- readFile' s = do
--   raw <- BL.split (c2w '\n') <$> BL.readFile s
--   return $ map (toPlainText' ' ') raw

-- importTexts' :: Char          -- ^ a text separator
--             -> BL.ByteString -- ^ the input string
--             -> PlainText
-- importTexts' sep xs = foldr step mempty (BL.split (c2w sep) xs)
--   where
--     step :: BL.ByteString -> PlainText -> PlainText
--     step t acc = mappend (PlainText 1 [read $ show t]) acc
{-
not working:

λ> let t = "1 2 3" :: BL.ByteString
λ> BL.split (c2w ' ') t
["1","2","3"]
λ> mapM show $BL.split (c2w ' ') t
["\"\"\"","\"\"3","\"\"\"","\"2\"","\"23","\"2\"","\"\"\"","\"\"3","\"\"\"","1\"\"","1\"3","1\"\"","12\"","123","12\"","1\"\"","1\"3","1\"\"","\"\"\"","\"\"3","\"\"\"","\"2\"","\"23","\"2\"","\"\"\"","\"\"3","\"\"\""]

-}
