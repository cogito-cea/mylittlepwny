-----------------------------------------------------------------------------
-- |
-- Module      :  Traces
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- Main interface module for the management of traces.
--
-----------------------------------------------------------------------------

module Traces
  ( module Traces.Raw
  , module Traces.Text
  , TraceData(..)
  , TMin(..), TMax(..)
  , importTraces
  )
where

import qualified Data.Vector.Unboxed as U
import           Prelude             hiding (init)

import           Traces.Raw
import           Traces.Text
import           Traces.Internal

data TraceData = TracesDir FilePath
               | TraceRawFile FilePath
               -- TODO TracePico InitPico
               deriving (Show, Eq)

-- | A wrapper that hides the different modules that support traces
--   reading.  From a 'TraceData', this function will return a trace
--   handler, and a loadWindow function.
importTraces :: (Read a, Num a, U.Unbox a)
  => TraceData -> IO (TMin -> TMax -> IO (Trace a), Int)
importTraces traces = do
  case traces of
    TraceRawFile f -> do
      h <- init f :: IO HandleRaw
      return (loadWindow h, size h)
    -- TODO add support for traces in text format
    TracesDir d -> do
      h <- init d :: IO HandleText
      return (loadWindow h, size h)
