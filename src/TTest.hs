{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module TTest
  ( ttest
  , TTestOptions(..)
  ) where

import           Control.Monad                          (replicateM)
import           Data.Fold
import           Data.Maybe                             (fromMaybe)
import qualified Data.Text                              as T
import qualified Data.Vector.Unboxed                    as U
import           Formatting
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.FilePath.Posix                  (takeDirectory, (</>))
import           Text.Printf                            (printf)

import           CLI.Types
import qualified Folds                                  as F
import qualified Traces.Raw                             as Traces

default (T.Text)


-- | t-test analysis
ttest :: TTestOptions -> IO ()
ttest TTestOptions{..} = do

  -- importing data traces
  (h, tmax') <- case traces of
    TraceRawFile f -> do
      h <- Traces.init f
      return (h, Traces.size h)
    -- TODO add support for traces in text format
    TracesDir _ -> error "TODO -- unsupported trace format"

  let traceDir = case traces of
        TraceRawFile f -> takeDirectory f
        TracesDir d    -> d

  -- print some user settings
  let tmax = fromMaybe tmax' mtmax
  let fter :: Format r (T.Text -> Int -> r)
      fter = right 24 ' ' % left 10 ' ' % "\n"
  fprint "T-test settings: \n"
  fprint fter "Total number of traces: " nbTraces
  fprint fter "tmin: " tmin
  fprint fter "tmax: " tmax
  fprint "\n"

  -- Each trace is associated to a class, described by an Int number.
  -- The classesFile describes the class descriptor associated to each
  -- trace.
  --
  -- classes: a list of Int values in {0, 1}.
  classes <- map read . lines <$> readFile classesFile

  -- TTEST analysis
  --
  -- TODO we should take nbTraces for each population,
  --  i.e.,
  --    - reading lazily the traces
  --    - sorting the two populations, lazily
  --    - take nbTraces in each population
  --  In this case, set the default value of nbTraces to 10000 (in CLI).
  ts <- replicateM nbTraces $ Traces.load' h tmin tmax

  let partition :: L' (Int, Traces.Trace Float) ([Traces.Trace Float], [Traces.Trace Float])
      partition = L' done step begin
      begin = ([], [])
      step :: ([a], [a]) -> (Int, a) -> ([a], [a])
      step (p0, p1) (c, x) =
        if (c == 0)
        then (p0 ++ [x], p1)
        else (p0, p1 ++ [x])
      done = id

  let (pop0, pop1) = flip run partition $ zip classes ts
      tvals = flip run F.ttestU $ zip pop0 pop1

  -- plot t-test results
  -- ----------------
  let plotOpts = PlotTTest traceDir nbTraces tmin tmax
  plotTTest plotOpts tvals
  return ()

data PlotTTest = PlotTTest
  { plotDir  :: FilePath
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  }

-- | TODO
plotTTest :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotTTest -> (U.Vector a) -> IO ()
plotTTest PlotTTest{..} ts = do
  let tmin = fromIntegral plotTmin
      tmax = fromIntegral plotTmax
      abscissa = [tmin..] :: [Float]
      graphFile = plotDir
                  </> printf "TTest n:%d tmin:%05d tmax:%05d.png" plotSize plotTmin plotTmax
  fprint ("\nRendering the TTest plot in: " % string % " \n") graphFile
  toFile def graphFile $ do
    layout_title .= printf "Non-specific t-test. Computation for %d traces." plotSize
    setColors [ opaque grey, opaque black]
    plot $ line "t-value" $ [ zip abscissa $ U.toList $ ts ]
    setColors [ opaque blue, opaque blue]
    plot $ line "" $ [ [(tmin,  4.5), (tmax,  4.5)]
                     , [(tmin, -4.5), (tmax, -4.5)]
                     ]

-- * CLI options
data TTestOptions = TTestOptions
  { traces      :: !TraceData
  , tmin        :: !Int          -- ^ the number of the first sample used
  , mtmax       :: !(Maybe Int)  -- ^ the number of the latest sample used
  , nbTraces    :: !Int          -- ^ number of traces used for the CPA analysis
  , classesFile :: !FilePath
  } deriving (Show)
