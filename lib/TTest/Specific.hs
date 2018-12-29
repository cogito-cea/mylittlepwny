{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

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
-- Specific t-test.  Implementation of the command 'specific'
--
-----------------------------------------------------------------------------


module TTest.Specific
  ( ttestSpecific
  , TTestSpecificOptions(..)

  -- CLI
  , cmdTTestSParser
  ) where

import           Conduit
import           Data.Bits                              (Bits, shiftR, (.&.))
import           Data.Maybe                             (fromMaybe)
import qualified Data.Text                              as T
import qualified Data.Vector.Unboxed                    as U
import           Data.Word                              (Word8)
import           Formatting
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative
import           System.FilePath.Posix                  (takeDirectory, (</>))
import           Text.Printf                            (printf)

import           Aes
import           Aes.Hypothesis
import           AesImport
import           CLI.Internal
import           Traces                                 (TMax (TMax),
                                                         TMin (TMin))
import qualified Traces                                 as Traces
import           TTest.Common

default (T.Text)


-- | t-test analysis
ttestSpecific :: TTestSpecificOptions -> IO ()
ttestSpecific TTestSpecificOptions{..} = do
  -- importing data traces
  (loadfun, tmax') <- Traces.importTraces traces

  let traceDir = case traces of
        TraceRawFile f -> takeDirectory f
        TracesDir d    -> d

  -- print some user settings
  let tmax = fromMaybe tmax' mtmax
  let fter :: Format r (T.Text -> Int -> r)
      fter = right 24 ' ' % left 10 ' ' % "\n"
  fprint "Specific t-test.  Settings: \n"
  fprint fter "Total number of traces: " nbTraces
  fprint fter "tmin: " tmin
  fprint fter "tmax: " tmax
  fprint fter "Specific t-test on bit: " targetBit
  fprint "\n"

  -- t-test analysis
  -- ---------------

  -- state hypothesis, built from the knowledge of the plaintext and key values
  key <- importKey keyFile

  let targetByte = targetBit `div` 8
      targetBitInByte = targetBit `rem` 8
      keyByte = getByte targetByte $ toAesText key
      getBit :: (Bits a, Num a) => Int -> a -> a
      getBit b w = w `shiftR` b .&. 0x1

      stateHypothesis :: Word8  -- ^ key byte
                      -> Plaintext
                      -> Word8  -- ^ bit value in the output of the first sbox
      stateHypothesis k t = getBit targetBitInByte $ fstSBOX'' targetByte k t

  -- List of texts.  Our input file is a rather small file, so it may
  -- no be necessary to load it in the conduit stream.
  texts <- take nbTraces <$> importTexts textFile

  let loadTraces :: MonadIO m => ConduitT () (Traces.Trace Float) m ()
      loadTraces = repeatMC (liftIO $ loadfun (TMin tmin) (TMax tmax))

      loadTracePairs :: MonadResource m => ConduitT () (Pair Plaintext (Traces.Trace Float)) m ()
      loadTracePairs = getZipSource
                       $ Pair
                       <$> ZipSource (yield texts .| concatMapC id )
                       <*> ZipSource loadTraces

      -- TTTrace selectors
      separateTraces :: (Pair Plaintext (Traces.Trace Float)) -> TTTrace
      separateTraces (Pair x t) =
        case stateHypothesis keyByte x of
          0 -> TTTrace Pop0 t
          1 -> TTTrace Pop1 t
          -- we should never land here, but YMMV ;)
          _ -> error "TTest.Specific.separatetraces: unexpected bit value"

      buildTTestTraces :: Monad m => ConduitT (Pair Plaintext (Traces.Trace Float)) TTTrace m ()
      buildTTestTraces = mapC separateTraces

  tvalues <- runResourceT
             $ runConduit
             $ loadTracePairs
             .| takeC nbTraces
             .| buildTTestTraces
             .| ttest

  -- plot t-test results
  -- ----------------
  let plotOpts = PlotTTest traceDir nbTraces tmin tmax targetBit
  plotTTest plotOpts tvalues

-- a strict pair
data Pair a b = Pair !a !b


data PlotTTest = PlotTTest
  { plotDir  :: FilePath
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  , bitNb    :: Int
  }

-- * plot the t-test results

plotTTest :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotTTest -> (U.Vector a) -> IO ()
plotTTest PlotTTest{..} ts = do
  let tmin = fromIntegral plotTmin
      tmax = fromIntegral plotTmax
      abscissa = [tmin..] :: [Float]
      graphFile = plotDir
                  </> printf "TTest-S n:%d tmin:%05d tmax:%05d b:%03d.png" plotSize plotTmin plotTmax bitNb
  fprint ("\nRendering the TTest plot in: " % string % " \n") graphFile
  toFile def graphFile $ do
    layout_title .= printf "Specific t-test - output of the first SBOX on bit %d. Computation for %d traces." bitNb plotSize
    setColors [ opaque grey, opaque black]
    plot $ line "t-value" $ [ zip abscissa $ U.toList $ ts ]
    setColors [ opaque blue, opaque blue]
    plot $ line "" $ [ [(tmin,  4.5), (tmax,  4.5)]
                     , [(tmin, -4.5), (tmax, -4.5)]
                     ]

-- * CLI options

data TTestSpecificOptions = TTestSpecificOptions
  { traces    :: !TraceData
  , tmin      :: !Int          -- ^ the number of the first sample used
  , mtmax     :: !(Maybe Int)  -- ^ the number of the latest sample used
  , nbTraces  :: !Int          -- ^ number of traces used for the CPA analysis
  , textFile  :: !FilePath
  , keyFile   :: !FilePath
  , targetBit :: !Int -- targetting only the output of the first SBOX
                      -- MAYBE add support for other target bits
  } deriving (Show)

cmdTTestSParser :: Parser TTestSpecificOptions
cmdTTestSParser =
  TTestSpecificOptions
  <$> parseTraces
  <*> parseTmin
  <*> parseTmax
  <*> parseNbTraces 20000
  <*> parseTextFile
  <*> parseKeyFile
  <*> ( option (fromInteger <$> auto)
        ( long "target-bit" <> short 'b'
          <> metavar "BIT"
          <> help "single-bit t-test on bit #BIT in the output of the first SBOX"
        )
      )
