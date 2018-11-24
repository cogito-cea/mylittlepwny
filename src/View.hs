{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module View
  ( viewTraces
  , ViewOptions(..)

  -- CLI
  , cmdViewParser
  ) where

import           Conduit
import           Data.Maybe                             (fromMaybe)
import qualified Data.Text                              as T
import qualified Data.Vector.Unboxed                    as U
import           Formatting
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative
import           System.FilePath.Posix                  (takeDirectory, (</>))
import           Text.Printf                            (printf)

import           CLI.Internal
import qualified Traces                                 as Traces

default (T.Text)

viewTraces :: ViewOptions -> IO ()
viewTraces ViewOptions{..} = do
  -- importing data traces
  (loadfun, tmax') <- Traces.importTraces traces

  let traceDir = case traces of
        TraceRawFile f -> takeDirectory f
        TracesDir d    -> d

  -- print some user settings
  let tmax = fromMaybe tmax' mtmax
  let fter :: Format r (T.Text -> Int -> r)
      fter = right 20 ' ' % left 10 ' ' % "\n"
  fprint "View Settings: \n"
  fprint fter "average over: " avgSize
  fprint fter "tmin: " tmin
  fprint fter "tmax: " tmax
  fprint fter "first trace: " firstTrace
  fprint "\n"

  -- load and process traces
  let loadTraces :: MonadIO m => ConduitT () (Traces.Trace Float) m ()
      loadTraces = repeatMC (liftIO $ loadfun tmin tmax)

  trace <- runConduit
    $ loadTraces
    -- Note: dropC and takeC are sequentially composed instead of
    -- using the conduit operator '.|'.
    --
    -- cf. conduit notes: since this function doesn't produce
    -- anything, you probably want to use it with (>>) instead of
    -- directly plugging it into a pipeline:
    .| (dropC firstTrace >> takeC avgSize)
    .| average (tmax - tmin)

  plotTraces (PlotOpts traceDir avgSize firstTrace tmin tmax) trace

average :: (Monad m)
  => Int    -- ^ the number of samples per traces
  -> ConduitT (Traces.Trace Float) Void m (Traces.Trace Float)
average n =
    getZipSink (go <$> ZipSink sumU <*> ZipSink lengthC)
  where
    go :: U.Vector Float -> Int -> U.Vector Float
    go total len = U.map (/ (fromIntegral len)) total
    sumU = foldlC (U.zipWith (+)) (U.replicate n 0)

data PlotOpts = PlotOpts
  { plotDir  :: FilePath
  , plotAvg  :: !Int
  , plotFst  :: !Int
  , plotTmin :: !Int
  , plotTmax :: !Int
  }

-- | plot Pearson's correlation.  Computation for n traces.
--
-- MAYBE use conduit
--    plotTraces :: (...) => PlotOpts -> ConduitT (Trace a) () m ()
plotTraces :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotOpts -> Traces.Trace a -> IO ()
plotTraces PlotOpts{..} ts = do
  let graphFile = plotDir
                  </> printf "Traces avg:%d n:%d tmin:%05d tmax:%05d.png" plotAvg plotFst plotTmin plotTmax
  fprint ("\nRendering the view plot in: " % string % " \n") graphFile
  toFile def graphFile $ do
    layout_title .= printf "Observation traces.  Average over %d traces." plotAvg
    setColors [ opaque grey, opaque black]
    let abscissa = [(fromIntegral plotTmin)..] :: [Float]
    plot $ line "Side-channel trace" $ [ zip abscissa $ U.toList ts ]

-- * CLI Options
data ViewOptions = ViewOptions
  { traces     :: !TraceData
  , tmin       :: !Int          -- ^ the number of the first sample used
  , mtmax      :: !(Maybe Int)  -- ^ the number of the latest sample used
  , avgSize    :: !Int          -- ^ number of traces used for the CPA analysis
  , firstTrace :: !Int -- ^ number of the first trace to consider
  } deriving (Show)

cmdViewParser :: Parser ViewOptions
cmdViewParser =
  ViewOptions
  <$> parseTraces
  <*> parseTmin
  <*> parseTmax
  <*> option (fromInteger <$> auto)
  ( long "average-over" <> short 'n'
    <> metavar "AVG_SIZE"
    <> help "Compute an average over AVG_SIZE traces [default: 1]"
    <> value 1
  )
  <*> option (fromInteger <$> auto)
  ( long "first-trace" <> short 'm'
    <> metavar "FIRST_TRACES"
    <> help "Number of the first trace to consider [default: 0]"
    <> value 0
  )
