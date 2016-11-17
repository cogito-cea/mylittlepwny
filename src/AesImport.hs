module AesImport
  ( Text
  , ciphertext
  , key
  , plaintext
  , toText
  , exportTexts
  , importTexts
  ) where

import           Data.Char (isSpace)
import           Data.List (dropWhileEnd)
import           Data.Word

import           Aes.Types

-- | a plaintext is a sized list of bytes
data Text = Text
  Int     -- ^ the size of the plaintext, in number of bytes
  [Word8] -- ^ the plaintext bytes
  deriving (Show)

instance Monoid Text where
  mempty = Text 0 []
  mappend (Text n t) (Text n' t') =
    Text (n+n') (t ++ t')
  mconcat = foldr mappend mempty

-- | file import, with Strings.
importTexts :: FilePath   -- ^ the filename of the input plaintexts file
           -> IO [Text]
importTexts f = do
  raw <- lines <$> Prelude.readFile f
  return $ map toText raw

-- | file export.
exportTexts :: FilePath   -- ^ the filename of the output file
            -> [Text]
            -> IO ()
exportTexts f ts = Prelude.writeFile f $ unlines $ map fromText ts

-- | conversion of a 'Text' data structure to 'Plaintext'.
plaintext :: Text -> Plaintext
plaintext (Text _ ts) = Plaintext ts

-- | conversion of a 'Text' data structure to 'Ciphertext'.
ciphertext :: Text -> Ciphertext
ciphertext (Text _ ts) = Ciphertext ts

-- | create a 'Text' from a textual string representation.
--   Expected string format: decimal integers separated by space characters.
toText :: String    -- ^ the input string
       -> Text
toText xs = foldr step mempty (words xs)
  where
    step :: String -> Text -> Text
    step t = mappend (Text 1 [read t])

-- | Create a textual 'String' representation from a 'Text'.
fromText :: Text -> String
fromText (Text _ ts) = dropWhileEnd isSpace $ foldr step "" ts
  where
    step :: Word8 -> String -> String
    step w x = show w ++ " " ++ x

-- | create an AES 'Key'.  Calls 'error' if the input Text is not correctly sized.
-- TODO: key :: Text -> Maybe Key
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
