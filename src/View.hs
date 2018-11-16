{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module View
  ( viewTraces
  ) where

import           Data.Maybe                             (fromMaybe)
import qualified Data.Text                              as T
import qualified Data.Vector.Unboxed                    as U
import           Formatting
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.FilePath.Posix                  (takeDirectory, (</>))
import           Text.Printf                            (printf)

import           CLI
import qualified Traces.Raw                             as Traces

default (T.Text)

-- TODO use a streaming library to avoid loading *all* the traces (we load from an IO action)

viewTraces :: ViewOptions -> IO ()
viewTraces ViewOptions{..} = do
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
  let nbTraces = 10
  let fter :: Format r (T.Text -> Int -> r)
      fter = right 20 ' ' % left 10 ' ' % "\n"
  fprint "CPA Settings: \n"
  fprint fter "number of traces: " nbTraces
  fprint fter "tmin: " tmin
  fprint fter "tmax: " tmax
  fprint "\n"

  -- CPA analysis
  let ts = take nbTraces $ Traces.load' h tmin tmax

  -- plot CPA results
  -- ----------------

  let plotOpts = PlotOpts traceDir nbTraces tmin tmax
  plotTraces plotOpts ts


data PlotOpts = PlotOpts
  { plotDir  :: FilePath
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  }

-- | plot Pearson's correlation.  Computation for n traces.
plotTraces :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotOpts -> [Traces.Trace a] -> IO ()
plotTraces PlotOpts{..} ts = do
  let graphFile = plotDir
                  </> printf "Traces n:%d tmin:%05d tmax:%05d.png" plotSize plotTmin plotTmax
  fprint ("\nRendering the CPA plot in: " % string % " \n") graphFile
  toFile def graphFile $ do
    layout_title .= printf "TODO TODO: %d." plotSize
    setColors [ opaque grey, opaque black]
    let abscissa = [(fromIntegral plotTmin)..] :: [Float]
    plot $ line "Wrong key hypotheses" $ [ zip abscissa $ U.toList c | c <- ts ]
