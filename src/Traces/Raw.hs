{-

-}

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

import           Control.Applicative  (many)
import           Control.Monad        (replicateM)
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Unboxed  as U


type Trace a = U.Vector a

data Handle = Handle
  { rawStream :: BSL.ByteString
  , traceSize :: !Int
  }

{-| The trace file has the following format:
+ data header
  + uint32: number of samples per trace
  + uint32: size in bytes of each sample
+ stream of data samples
-}

-- | return a 'Handle' on the stream of side-channel traces
init :: FilePath -> IO Handle
init filename = do
  f <- BSL.readFile filename
  let s =  fromIntegral $ runGet getInt32host f
  return $ Handle f s

-- | Read the data header, return the number of samples per trace
size :: Handle -> Int
size Handle{..} = traceSize

-- | Load lazily the list of traces.
load :: Handle -> [Trace Float]
load Handle{..} = flip runGet rawStream $ do
  -- the number of samples
  _ <- getInt32host
  -- samples size
  _ <- getInt32host
  -- the list of traces
  many $ getTrace traceSize

  where
    getTrace :: Int -> Get (Trace Float)
    getTrace n = U.fromList . map fromIntegral <$> replicateM n getInt16host

-- | Load lazily the list of traces.
load' :: Handle
      -> Int     -- ^ 'tmin'. Index of the first sample in the observation window
      -> Int     -- ^ 'tmax'. 'tmax-1' is the index of the last sample in the observation window
      -> [Trace Float]
load' Handle{..} tmin tmax =
  let
    getTrace :: Int -> Get (Trace Float)
    getTrace n = U.fromList . map fromIntegral . take (tmax-tmin) . drop (tmin-1) <$> replicateM n getInt16host
  in
    flip runGet rawStream $ do
    -- the number of samples
    _ <- getInt32host
    -- samples size
    _ <- getInt32host
    -- the list of traces
    many $ getTrace traceSize


-- | Close the trace handler.  Actually, this function does nothing.
close :: Handle -> IO ()
close _ = return ()
