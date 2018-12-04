{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Traces.PicoScope.Stm32
  ( Handle
  , Config(..)
  , Traces.PicoScope.Stm32.init
  , Traces.PicoScope.Stm32.close
  , Traces.PicoScope.Stm32.load
  , readCipher
  , sendText
  )
  where


-- MAYBE.  replace Control.Foldl with shelly
import qualified Control.Foldl              as Fold
import           Control.Monad              (void)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified System.Hardware.Serialport as Serial
import qualified System.IO                  as IO
import           System.Posix.IO.ByteString (closeFd, fdRead)


import           System.Posix.Types         (Fd)
import           Turtle                     (fromString, printf, w, (%))
import qualified Turtle                     as Tt

import           Aes
import           AesImport


-- | module configuration settings.
data Config = Config { binFile    :: Tt.FilePath            -- ^ Location of the program file that will be loaded on the STM32 (bin format).
                     , flashExe   :: Maybe Tt.FilePath      -- ^ Location of the flashing program [default: 'st-flash'].
                     , deviceFile :: Maybe Prelude.FilePath -- ^ Location of the dev file to communicate with the STM32
                     -- TODO comm speed
                     }
  deriving (Show)

-- | the connexion handle
data Handle = Handle
  { p       :: Serial.SerialPort
  , h       :: IO.Handle
  , stflash :: Tt.FilePath
  }

-- | Reads the file descriptor.  If @msg@ is not correctly received,
--   close the file descriptor and exit the program.
expectOn :: Fd -> String -> IO ()
expectOn fd msg = do
  let l = length msg
  (res, _n) <- fdRead fd $ fromIntegral l
  if res == msg
    then return ()
    else do
      closeFd fd
      printf ("ERROR.  Received: "%w%" instead of the expected message\n") res

-- | Load the binary program on the STM32, and setup a running
--   connexion with the STM32.
init :: Config -> IO (Either String Handle)
init (Config binProg mstflash mdev) = do
  -- location of the st-flash program
  stflash <- case mstflash of
    Just f -> return f
    Nothing -> findStFlash
  printf ("[Stm32] location of the st-flash tool: "%w%"\n") stflash

  -- location of the device file for serial communication.  Default is "/dev/ttyUSB0"
  let dev = fromMaybe "/dev/ttyUSB0" mdev

  h <- Serial.hOpenSerial dev Serial.defaultSerialSettings { Serial.commSpeed = Serial.CS115200 }
  p <- Serial.openSerial  dev Serial.defaultSerialSettings { Serial.commSpeed = Serial.CS115200 }

  -- load the binary file
  Tt.echo "[Stm32] Loading the binary file..."
  _ <- Tt.proc (Tt.format Tt.fp stflash)
       ["--reset", "write", Tt.format Tt.fp binProg, "0x8000000"]
       mempty
  Tt.echo "[Stm32]                        ... done."

  -- startup préambule
  Tt.echo "[Stm32] Wait for target init messages..."
  _ <- IO.hGetLine h -- "STM32 isSTM32 is alive!\r"
  _ <- IO.hGetLine h -- "uart 1 is configured with 115200 bauds\r"

  _ <- IO.hGetLine h -- first fake cipher text. "\CAN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"

  -- the STM32 is ready to read plaintexts.
  Tt.echo "[Stm32]                              ... done."

  Tt.echo "[Stm32] init done."
  return $ Right $ Handle p h stflash


-- | close everything.
close :: Handle -> IO ()
close Handle{..} = do
  IO.hClose h
  Serial.closeSerial p


-- | find the st-flash utility.
--
-- TODO better manage errors.
-- TODO function signature should be:
-- findStFlash :: Tt.MonadIO io => io (Either String Tt.FilePath)
findStFlash :: Tt.MonadIO io => io Tt.FilePath
findStFlash = do
  res <- Tt.fold (Tt.find (Tt.has "st-flash") ".") Fold.list
  if length res /= 1
    then error $ "Possible values for filepath to the 'st-flash' executable: " ++ show res
    else return $ res !! 1

{- | Read a ciphertext value on the UART from the STM32.  The function
   polls on the serial port until a whole cipher text is received (17
   bytes expected).

17 bytes are expected from the STM32 as follows:
   chr 0x18 : [b0 .. b15]

We drop the first byte and return the following:
   Ciphertext [b0 .. b15]
-}
readCipher :: Handle -> IO Ciphertext
readCipher h = fromAesText . toAesText . B.drop 1 <$> get (Serial.recv (p h)) 17 C.empty
  where
    get :: (Int -> IO C.ByteString) -> Int -> C.ByteString -> IO C.ByteString
    get _ 0 xs = return xs
    get f n xs = do
      xs' <- f n
      let n' = C.length xs'
      get f (n-n') (xs <> xs')

sendText :: Handle -> Plaintext -> IO ()
sendText h txt = let serialTxt = startReceiving <> (fromAesText . toAesText) txt
                 in  void $ Serial.send (p h) serialTxt

startReceiving :: B.ByteString
startReceiving = fromString [chr 0x17]

-- | load a new binary file
load :: Handle -> FilePath -> IO ()
load Handle{..} f = do
  -- flush the serial port  (non-blocking)
  x <- Serial.recv p 256
  print x

  -- load the new bin file
  Tt.echo "[Stm32] Loading the binary file..."
  _ <- Tt.proc (Tt.format Tt.fp stflash)
       ["--reset", "write", T.pack f, "0x8000000"]
       mempty
  Tt.echo "[Stm32]                        ... done."

  -- startup préambule
  Tt.echo "[Stm32] Wait for target init messages..."
  _ <- IO.hGetLine h -- "STM32 isSTM32 is alive!\r"
  _ <- IO.hGetLine h -- "uart 1 is configured with 115200 bauds\r"

  _ <- IO.hGetLine h -- first fake cipher text. "\CAN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"

  return ()
