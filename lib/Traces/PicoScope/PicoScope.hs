{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Traces.PicoScope.PicoScope
  ( Config(..)
  , Handle
  , Traces.PicoScope.PicoScope.close
  , Traces.PicoScope.PicoScope.init
  , Traces.PicoScope.PicoScope.read
  , Traces.PicoScope.PicoScope.ready
  , expectOn
  )
  where

import           Control.Monad              (replicateM)
import           Data.Binary.Get
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as BL
import           Data.Int                   (Int16)
import           System.Posix.IO.ByteString (OpenMode (..), closeFd,
                                             defaultFileFlags, fdRead,
                                             fdReadBuf, openFd)
import           System.Posix.Types         (Fd)
import           System.Process             (ProcessHandle, runProcess,
                                             terminateProcess)

import           Filesystem.Path.CurrentOS  (encodeString)
import           Foreign                    (Ptr, Word8)
import           Foreign.Marshal.Alloc      (free, mallocBytes)
import           Foreign.Marshal.Array      (peekArray)
import           Turtle                     (printf, w, (%))
import qualified Turtle                     as Tt


ready :: String
ready = "READY\n"

data Config = Config
  { fifo      :: C.ByteString
  , nbTraces  :: Int
  , nbSamples :: Int
  } deriving (Show)

data Handle = Handle
  { fd         :: Fd
  , tracebuf   :: Ptr Word8
  , bufferSize :: Int
  , traceSize  :: Int
  , pico       :: ProcessHandle
  }


-- | Reads the file descriptor.  If @msg@ is not correctly received,
--   close the file descriptor and exit the program.
--
--   TODO should we terminate or only report an error?
expectOn :: Handle -> String -> IO ()
expectOn Handle{..} msg = expectOn' fd msg
expectOn' :: Fd -> String -> IO ()
expectOn' fd msg = do
  let l = length msg
  (res, _n) <- fdRead fd $ fromIntegral l
  if res == msg
    then return ()
    else do
      printf ("ERROR.  Received: "%w%" instead of the expected message\n") res

-- | read a trace from the PicoScope
read :: Handle -> IO [Int16]
read Handle{..} = do
  -- Don't use fdRead here: fdRead does a conversion to String *using
  -- the locale encoding*, which will mess our raw data.
  n <- fdReadBuf fd tracebuf (fromIntegral bufferSize)
  clbuf <- peekArray (fromIntegral n) tracebuf

  -- conversion [Word8] -> BL.ByteString -> [Int16]
  return $ runGet (getBuffer traceSize) (BL.pack clbuf)
  where
    getBuffer :: Int -> Get [Int16]
    getBuffer n = replicateM n getInt16host

-- | setup the communication with the PicoScope
init :: Config -> IO Handle
init Config{..} = do
  Tt.echo "[PicoScope] Loading the control program..."
  fifopath <- Tt.realpath $ Tt.fromString $ C.unpack fifo
  picoControl <- Tt.realpath $ Tt.fromString $ "./utils/pico-control/pico_controller"

  pico <- runProcess (encodeString picoControl)
                     [ "--fifo", encodeString fifopath
                     , "--dump-in-fifo"
                     , "--nb-traces", show nbTraces
                     , "--nb-samples", show nbSamples
                     ]
                     Nothing  -- Optional path to the working directory
                     Nothing  -- Optional environment (otherwise inherit)
                     Nothing  -- Handle to use for stdin (Nothing => use existing stdin)
                     Nothing  -- Handle to use for stdout (Nothing => use existing stdout)
                     Nothing  -- Handle to use for stderr (Nothing => use existing stderr)
  Tt.echo "[PicoScope]                            ... done."

  -- open the fifo
  fp <- openFd fifo ReadOnly Nothing defaultFileFlags

  -- at init, a first READY string is expected
  expectOn' fp ready

  let bufferSize = nbSamples*2 -- TODO avec le support Data.Word pour un calcul correct de la taille du buffer
  tracebuf <- mallocBytes bufferSize

  return $ Handle fp tracebuf bufferSize nbSamples pico

-- | Close everything
close :: Handle -> IO ()
close Handle{..} = do
  free tracebuf
  closeFd fd
  terminateProcess pico
