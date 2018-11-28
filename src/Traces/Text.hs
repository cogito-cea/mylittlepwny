{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Traces.Text
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple interface for loading side-channel traces from a set of text files.
--
-----------------------------------------------------------------------------

module Traces.Text
  ( HandleText
  , HasTraces
  ) where

import           Control.Exception     (evaluate)
import           Data.IORef            (IORef, modifyIORef', newIORef,
                                        readIORef)
import           Data.List.Split       (splitOn)
import qualified Data.Vector.Unboxed   as U
import           System.FilePath.Posix ((</>))
import           Text.Printf           (printf)

import           Traces.Internal

instance HasTraces HandleText where
  init = Traces.Text.init
  close = Traces.Text.close
  load = Traces.Text.load
  load' = Traces.Text.load'
  size = Traces.Text.size


data HandleText = HandleText
  { tracesDir :: !FilePath
  , counter   :: IORef Int
  , sampleNb  :: !Int  -- ^ trace size, in elements
  }

{-| The trace file has the following format:
+ data header
  + uint32: number of samples per trace
  + uint32: size in bytes of each sample
+ stream of data samples
-}

-- | Return a 'Handle' on the stream of side-channel traces
init :: FilePath -> IO HandleText
init tdir = do
  -- read the number of samples per trace, from the first file
  raw <- readFile $ tdir </> printf "trace_%09d.txt" (0 :: Int)
  -- Count only the number of elements, don't try to interpret float values
  -- MAYBE - read with the text package
  let nb = length $ takeWhile (not . null) $ splitOn " " raw

  ref <- newIORef 0
  return $ HandleText tdir ref nb

close :: HandleText -> IO ()
close _ = return ()

-- | Load a new trace.
load :: (Read a, U.Unbox a) => HandleText -> IO (Trace a)
load (HandleText tdir nref _nb) = do
  n <- readIORef nref
  raw <- readFile $ tdir </> printf "trace_%09d.txt" n
  -- MAYBE - read with the text package
  let xs = map read $ takeWhile (not . null) $ splitOn " " raw
  modifyIORef' nref (+1)
  return $ U.fromList xs

-- | Return the number of samples per trace
size :: HandleText -> Int
size HandleText{..} = sampleNb

-- | Load one trace, filter samples out of the observation window.
--
-- MAYBE. use Conduit?
--   load' :: Handle -> Int -> Int -> ConduitT () (Trace Float) m ()
load' :: (Read a, U.Unbox a)
      => HandleText
      -> Int     -- ^ 'tmin'. Index of the first sample in the observation window
      -> Int     -- ^ 'tmax'. 'tmax-1' is the index of the last sample in the observation window
      -> IO (Trace a)
load' (HandleText tdir nref _nb) tmin tmax = do
  n <- readIORef nref
  -- MAYBE - read with the text package
  raw <- readFile $ tdir </> printf "trace_%09d.txt" n
  let xs = map read $ takeWhile (not . null) $ splitOn " " raw
  modifyIORef' nref (+1)
  -- force evaluation in WNHF so that the file descriptor is closed.
  -- otherwise we would get: "openFile: resource exhausted (Too many
  -- open files)"
  evaluate $ U.fromList $ take (tmax - tmin) . drop (tmin-1) $ xs
