{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TTest.Internal where


import           Conduit
import qualified Data.Vector.Unboxed                    as U

import qualified Traces                                 as Traces


data Trace = Trace Int (Traces.Trace Float)
  deriving (Eq, Show)

-- | Compute the t-test vector from a stream of 'Trace's
ttest :: Monad m
  => (Trace -> Bool) -- ^ trace selector for building the first population of traces
  -> (Trace -> Bool) -- ^ trace selector for building the second population of traces
  -> ConduitT Trace Void m (U.Vector Float)
ttest pop0 pop1 = getZipSink (go <$> population0 <*> population1)
  where
    go (Stats n0 m0 s0) (Stats n1 m1 s1) =
      -- TODO check that n0 == n1
      U.zipWith (/) (U.zipWith (-) m0 m1)
                    (U.map sqrt (U.map (/ (fromIntegral $ n0 * n1)) (U.zipWith (+) s0 s1)))

    -- build the partial statistics for each population of traces
    population0, population1 :: Monad m => ZipSink Trace m Stats
    population0 = ZipSink $ statsBuilder pop0
    population1 = ZipSink $ statsBuilder pop1

    -- Filter traces with the predicate function, and compute the
    -- partial ttest statistics.
    statsBuilder :: Monad m => (Trace -> Bool) -> ConduitT Trace o m Stats
    statsBuilder p = filterC p .| foldlC foldStats emptyStats

-- | Accumulator of our statistics that will be used for each
--   population of traces.
data Stats = Stats
             !Int              -- n
             !(U.Vector Float) -- m
             !(U.Vector Float) -- s
emptyStats :: Stats
emptyStats = Stats 0 U.empty U.empty
{-# INLINABLE emptyStats #-}

-- | Compute the t-test statistics iteratively: this function computes
--   the partial average and the partial standard deviation for one
--   population of traces.
--
-- The interface of this function is designed to be used with
-- 'foldlC'.
foldStats :: Stats           -- ^ initial state
          -> Trace           -- ^ the new trace
          -> Stats           -- ^ the new state
foldStats (Stats 0 _ _) (Trace _ x) = Stats 1 x zeros
  where
    zeros = U.map (const 0) x -- Assuming the two vectors have the same size
foldStats (Stats n m s) (Trace _ x) = Stats (n+1) m' s'
  where
    m' = U.zipWith (+) m $ U.map (/ (fromIntegral n)) $ U.zipWith (-) x m
    s' = U.zipWith (+) s $ U.zipWith (*) (U.zipWith (-) x m) (U.zipWith (-) x m')
{-# INLINABLE foldStats #-}
