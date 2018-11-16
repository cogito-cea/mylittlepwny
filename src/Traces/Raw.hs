{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Traces.Raw
-- Copyright   :  (c) CEA, 2018
-- License     :  see LICENSE
--
-- Maintainer  :  damien.courousse@cea.fr
-- Stability   :  internal
-- Portability :
--
-- Simple interface for loading side-channel traces from a raw data file.


module Traces.Raw
  ( Handle
  , Trace
  , Traces.Raw.init
  , Traces.Raw.size
  , load
  , load'
  , close
  ) where

import           Control.Monad        (replicateM, when)
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed  as U
import qualified System.IO as IO


type Trace a = U.Vector a

data Handle = Handle
  { handle :: IO.Handle
  , sizeB :: !Int  -- ^ trace size, in bytes
  , sampleNb :: !Int  -- ^ trace size, in elements
  }

{-| The trace file has the following format:
+ data header
  + uint32: number of samples per trace
  + uint32: size in bytes of each sample
+ stream of data samples
-}

-- | Return a 'Handle' on the stream of side-channel traces
init :: FilePath -> IO Handle
init filename = do
  h <- IO.openFile filename IO.ReadMode

  -- read the number of samples per trace
  s <- fromIntegral . runGet getInt32host <$> BL.hGet h 4

  -- check that the sample size is 2.  We don't handle other cases currently.
  ssize <- fromIntegral . runGet getInt32host <$> BL.hGet h 4
  when (ssize /= 2) $ error "Wrong data format.  The file header tells that the sample size is not 2."

  return $ Handle h (s*ssize) s

-- | Return the number of samples per trace
size :: Handle -> Int
size Handle{..} = sampleNb

-- | Load one trace
--
-- MAYBE. use Conduit?
--   load :: Handle -> ConduitT () (Trace Float) m ()
load :: Handle -> IO (Trace Float)
load Handle{..} = do
  raw <- BL.hGet handle sizeB
  return $ runGet getTrace raw
  where
    getTrace :: Get (Trace Float)
    getTrace = U.fromList . map fromIntegral <$> replicateM sampleNb getInt16host

-- | Load one trace, filter samples out of the observation window.
--
-- MAYBE. use Conduit?
--   load' :: Handle -> Int -> Int -> ConduitT () (Trace Float) m ()
load' :: Handle
      -> Int     -- ^ 'tmin'. Index of the first sample in the observation window
      -> Int     -- ^ 'tmax'. 'tmax-1' is the index of the last sample in the observation window
      -> IO (Trace Float)
load' Handle{..} tmin tmax = do
  raw <- BL.hGet handle sizeB
  return $ runGet getTrace raw
  where
    getTrace :: Get (Trace Float)
    getTrace = U.fromList . map fromIntegral . take (tmax-tmin) . drop (tmin-1) <$> replicateM sampleNb getInt16host

-- | Close the trace handler.  Actually, this function does nothing.
close :: Handle -> IO ()
close _ = return ()
