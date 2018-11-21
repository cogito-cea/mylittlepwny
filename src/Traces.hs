module Traces
  ( module Traces.Raw
  , module Traces.Text
  , TraceData(..)
  , importTraces
  )
where

import qualified Data.Vector.Unboxed as U
import           Prelude             hiding (init)

import           Traces.Raw
import           Traces.Text

data TraceData = TracesDir FilePath
               | TraceRawFile FilePath
               deriving (Show, Eq)

importTraces :: (Read a, Num a, U.Unbox a)
  => TraceData -> IO (Int -> Int -> IO (Trace a), Int)
importTraces traces = do
  case traces of
    TraceRawFile f -> do
      h <- init f :: IO HandleRaw
      return (load' h, size h)
    -- TODO add support for traces in text format
    TracesDir d -> do
      h <- init d :: IO HandleText
      return (load' h, size h)
