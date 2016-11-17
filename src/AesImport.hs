{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AesImport
  ( AesText
  , Byte
  , getByte
  , tow32

  , HasAesText(..)
  , exportTexts
  , importTexts
  ) where

import           Data.Char (isSpace)
import           Data.List (dropWhileEnd)
import           Data.Word

import           Aes.Types

-- | a generic data structure to handle import and export.
data AesText = AesText
  Int     -- ^ the size of the plaintext, in number of bytes
  [Word8] -- ^ the plaintext bytes
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
  -- | create a 'AesText' from a textual string representation.
  --   Expected string format: decimal integers separated by space characters.
  toAesText :: a -> AesText
  -- | Create a textual 'String' representation from a 'AesText'.
  fromAesText :: AesText -> a
  -- | Generalised import of 'String's. A decimal integer representation is expected.
  stringImport :: String -> a
  stringImport = fromAesText . toAesText
  -- | Generalised export to 'String's.
  exportString :: a -> String
  exportString = fromAesText . toAesText

instance HasAesText String where
  toAesText xs = foldr step mempty (words xs)
    where
      step :: String -> AesText -> AesText
      step t = mappend (AesText 1 [read t])
  fromAesText (AesText _ ts) = dropWhileEnd isSpace $ foldr step "" ts
    where step w x = show w ++ " " ++ x

instance HasAesText Key where
  toAesText = undefined
  fromAesText = key

instance HasAesText Plaintext where
  toAesText (Plaintext ts) = AesText (length ts) ts
  fromAesText (AesText _ ts) = Plaintext ts

instance HasAesText Ciphertext where
  toAesText (Ciphertext ts) = AesText (length ts) ts
  fromAesText (AesText _ ts) = Ciphertext ts

instance HasAesText State where
  toAesText (State s0 s1 s2 s3 _) =
    AesText 16 (octets s0 ++ octets s1 ++ octets s2 ++ octets s3)

  -- | a 'State' cannot be imported from text because we cannot define
  -- the 'KeySchedule' data without knowledge of the 'Key' value.  calling
  -- 'fromAesText' calls 'error'.
  --
  -- To avoid this, one would need to define two separate typeclasses
  -- for import and export.  'State' would only be an instance of the
  -- export class.
  fromAesText = undefined

-- | file import, with Strings.
importTexts :: FilePath   -- ^ the filename of the input plaintexts file
           -> IO [AesText]
importTexts f = do
  raw <- lines <$> Prelude.readFile f
  return $ map toAesText raw

-- | file export.
exportTexts :: FilePath   -- ^ the filename of the output file
            -> [AesText]
            -> IO ()
exportTexts f ts = Prelude.writeFile f $ unlines $ map fromAesText ts




-- | create an AES 'Key'.  Calls 'error' if the input AesText is not correctly sized.
-- TODO: key :: AesText -> Maybe Key
key :: AesText -> Key
key (AesText 16 bs) = Key128 $ RawKey $ tow32 bs
key (AesText 24 bs) = Key192 $ RawKey $ tow32 bs
key (AesText 32 bs) = Key256 $ RawKey $ tow32 bs
key (AesText n _)   = error $ "unknown key length: " ++ show n
-- | Convert each group of 4 'Word8's to a 'Word32'.
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
