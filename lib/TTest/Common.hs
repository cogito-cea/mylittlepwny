{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TTest.Common
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- T-test.  Common components for the specific and non-specific t-tests.
-----------------------------------------------------------------------------


module TTest.Common where


import           Conduit
import qualified Data.Vector.Unboxed as U

import qualified Traces              as Traces


data TTTrace = TTTrace { ttClass :: !Class
                       , ttTrace :: !(Traces.Trace Float)
                       } deriving (Eq, Show)

data Class = Pop0 | Pop1
  deriving (Eq, Show)

-- | Compute the t-test vector from a stream of 'Trace's
ttest :: Monad m => ConduitT TTTrace Void m (Traces.Trace Float)
ttest = getZipSink (closeStats <$> population0 <*> population1)
  where
    -- build the partial statistics for each population of traces
    population0, population1 :: Monad m => ZipSink TTTrace m Stats
    population0 = ZipSink $ statsBuilder pop0
    population1 = ZipSink $ statsBuilder pop1

    -- Filter traces with the predicate function, and compute the
    -- partial ttest statistics.
    statsBuilder :: Monad m => (TTTrace -> Bool) -> ConduitT TTTrace o m Stats
    statsBuilder p = filterC p
                     .| mapC ttTrace
                     .| foldlC foldStats emptyStats

    -- TTTrace selectors
    pop0, pop1 :: TTTrace -> Bool
    -- trace selector for building the first population of traces
    pop0 (TTTrace n _) = n == Pop0
    -- trace selector for building the second population of traces
    pop1 (TTTrace n _) = n == Pop1


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
foldStats :: Stats              -- ^ initial state
          -> Traces.Trace Float -- ^ the new trace
          -> Stats              -- ^ the new state
foldStats (Stats 0 _ _) x = Stats 1 x zeros
  where
    zeros = U.map (const 0) x -- Assuming the two vectors have the same size
foldStats (Stats n m s) x = Stats (n+1) m' s'
  where
    m' = U.zipWith (+) m $ U.map (/ (fromIntegral n)) $ U.zipWith (-) x m
    s' = U.zipWith (+) s $ U.zipWith (*) (U.zipWith (-) x m) (U.zipWith (-) x m')
{-# INLINABLE foldStats #-}

closeStats :: Stats -> Stats -> Traces.Trace Float
closeStats (Stats n0 m0 s0) (Stats n1 m1 s1) =
  -- MAYBE check that n0 == n1
  U.zipWith (/) (U.zipWith (-) m0 m1)
                (U.map sqrt (U.map (/ (fromIntegral $ n0 * n1)) (U.zipWith (+) s0 s1)))
{-# INLINABLE closeStats #-}
