{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Traces.Raw
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple interface for loading side-channel traces from a raw data file.
--
-----------------------------------------------------------------------------



module Traces.Raw
  ( HandleRaw
  , HasTraces(..)
  , Trace
  ) where

import           Control.Monad        (replicateM)
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed  as U
import qualified System.IO            as IO

import           Traces.Internal

instance HasTraces HandleRaw where
  type InitData HandleRaw = FilePath
  init = Traces.Raw.init
  close = Traces.Raw.close
  load = Traces.Raw.load
  loadWindow = Traces.Raw.loadW
  size = Traces.Raw.size

data HandleRaw = forall a. Integral a => HandleRaw
  { handle    :: IO.Handle
  , sizeB     :: !Int  -- ^ trace size, in bytes
  , sampleNb  :: !Int  -- ^ trace size, in elements
  , getSample :: Get a -- ^ the low-level function for reading sample values
  }

{-| The trace file has the following format:
+ data header
  + uint32: number of samples per trace
  + uint32: size in bytes of each sample
+ stream of data samples
-}

-- | Return a 'Handle' on the stream of side-channel traces
init :: FilePath -> IO HandleRaw
init filename = do
  h <- IO.openFile filename IO.ReadMode

  -- read the number of samples per trace
  s <- fromIntegral . runGet getWord32le <$> BL.hGet h 4

  -- check that the sample size is 2.  We don't handle other cases currently.
  ssize <- fromIntegral . runGet getWord32le <$> BL.hGet h 4
  putStrLn $ "Traces.Raw: ssize = " ++ show ssize
  return $ case ssize of
        1 -> HandleRaw h (s*ssize) s getWord8
        2 -> HandleRaw h (s*ssize) s getWord16le
        4 -> HandleRaw h (s*ssize) s getWord32le
        8 -> HandleRaw h (s*ssize) s getWord64le
        _ -> error "Traces.Raw:  Unsupported data format."

-- | Return the number of samples per trace
size :: HandleRaw -> Int
size HandleRaw{..} = sampleNb

-- | Load one trace
--
-- MAYBE. use Conduit?
--   load :: HandleRaw -> ConduitT () (Trace Float) m ()
load :: (U.Unbox a, Num a) => HandleRaw -> IO (Trace a)
load HandleRaw{..} = do
  raw <- BL.hGet handle sizeB
  return $ runGet getTrace raw
  where
    getTrace :: (U.Unbox a, Num a) => Get (Trace a)
    getTrace = U.fromList . map fromIntegral <$> replicateM sampleNb getSample

-- | Load one trace, filter samples out of the observation window.
--
-- MAYBE. use Conduit?
--   load' :: HandleRaw -> Int -> Int -> ConduitT () (Trace Float) m ()
loadW :: (U.Unbox a, Num a)
      => HandleRaw
      -> TMin     -- ^ 'tmin'. Index of the first sample in the observation window
      -> TMax     -- ^ 'tmax'. 'tmax-1' is the index of the last sample in the observation window
      -> IO (Trace a)
loadW HandleRaw{..} (TMin tmin) (TMax tmax) = do
  raw <- BL.hGet handle sizeB
  return $ runGet getTrace raw
  where
    getTrace :: (U.Unbox a, Num a) => Get (Trace a)
    getTrace = U.fromList . map fromIntegral . take (tmax - tmin) . drop (tmin-1) <$> replicateM sampleNb getSample

-- | Close the trace handler.  Actually, this function does nothing.
close :: HandleRaw -> IO ()
close _ = return ()
