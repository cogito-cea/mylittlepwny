{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TTest.NonSpecific
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- Non specific t-test.  Implementation of the command 'ttest'
--
-----------------------------------------------------------------------------

module TTest.NonSpecific
  ( ttestNonSpecific
  , TTestNonSpecificOptions(..)

  -- CLI
  , cmdTTestNSParser
  ) where

{- MAYBE remove use of the 'class file'.  Compare each plaintext with
   the usual static value - cf. TLVA.
-}

import           Conduit
import qualified Data.ByteString.Char8                  as BC
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
import           Traces                                 (TMax (TMax),
                                                         TMin (TMin))
import qualified Traces                                 as Traces
import           TTest.Common

default (T.Text)

-- | t-test analysis
ttestNonSpecific :: TTestNonSpecificOptions -> IO ()
ttestNonSpecific TTestNonSpecificOptions{..} = do
  -- prepare traces loading
  (loadfun, tmax') <- Traces.importTraces traces
  let traceDir = case traces of
        TraceRawFile f -> takeDirectory f
        TracesDir d    -> d

  -- print some user settings
  let tmax = fromMaybe tmax' mtmax
  let fter :: Format r (T.Text -> Int -> r)
      fter = right 24 ' ' % left 10 ' ' % "\n"
  let fters :: Format r (T.Text -> String -> r)
      fters = right 24 ' ' % left 10 ' ' % "\n"
  fprint "Non-specific t-test.  Settings: \n"
  fprint fter "Total number of traces: " nbTraces
  fprint fter "tmin: " tmin
  fprint fter "tmax: " tmax
  fprint fters "Classes description file: " classesFile
  fprint "\n"

  -- t-test analysis
  let loadTraces :: MonadIO m => ConduitT () (Traces.Trace Float) m ()
      loadTraces = repeatMC (liftIO $ loadfun (TMin tmin) (TMax tmax))

      loadClasses :: MonadResource m => ConduitT i Class m ()
      loadClasses = sourceFile classesFile
                     .| concatMapC (map toClass . BC.words)

      -- the file classesFile is loaded as a ByteString, which
      -- contains either chars 0 or 1.
      toClass :: BC.ByteString -> Class
      toClass "0" = Pop0
      toClass "1" = Pop1
      -- we should never land here, but YMMV ;)
      toClass _   = error "TTest.NonSpecific.toClass: unknown class value"

      loadTTestTraces :: MonadResource m => ConduitT () TTTrace m ()
      loadTTestTraces = getZipSource
        $ TTTrace
        <$> ZipSource loadClasses
        <*> ZipSource loadTraces

  tvalues <- runResourceT
             $ runConduit
             $ loadTTestTraces
             .| takeC nbTraces
             .| ttest

  -- plot t-test results
  let plotOpts = PlotTTest traceDir nbTraces tmin tmax
  plotTTest plotOpts tvalues


-- * plot the t-test results

data PlotTTest = PlotTTest
  { plotDir  :: FilePath
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  }

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
data TTestNonSpecificOptions = TTestNonSpecificOptions
  { traces      :: !TraceData
  , tmin        :: !Int          -- ^ the number of the first sample used
  , mtmax       :: !(Maybe Int)  -- ^ the number of the latest sample used
  -- MAYBE set the default value of nbTraces to 20000 (in CLI).
  , nbTraces    :: !Int          -- ^ number of traces used for the CPA analysis
  , classesFile :: !FilePath
  } deriving (Show)

cmdTTestNSParser :: Parser TTestNonSpecificOptions
cmdTTestNSParser =
  TTestNonSpecificOptions
  <$> parseTraces
  <*> parseTmin
  <*> parseTmax
  <*> parseNbTraces 20000
  <*> strOption
          ( long "classesFile" <> short 'c'
            <> metavar "CLASSESFILE"
            <> help "Location of the 'classes file'"
          )
