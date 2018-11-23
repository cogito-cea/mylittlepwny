{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module TTest.Specific
  ( ttestSpecific
  , TTestSpecificOptions(..)

  -- CLI
  , cmdTTestSParser
  ) where

import           Control.DeepSeq                        (force)
import           Control.Monad                          (replicateM)
import           Control.Parallel.Strategies
import           Data.Bits                              (Bits, shiftR, (.&.))
import           Data.Fold
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
import           Aes.Bits
import           Aes.Hypothesis
import           AesImport
import           CLI.Internal
import qualified Folds                                  as F
import qualified Traces                                 as Traces

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

  -- TTEST analysis
  --
  -- TODO we should take nbTraces for each population,
  --  i.e.,
  --    - reading lazily the traces
  --    - sorting the two populations, lazily
  --    - take nbTraces in each population
  --  In this case, set the default value of nbTraces to 10000 (in CLI).
  let partition :: L' (Word8, Traces.Trace Float) ([Traces.Trace Float], [Traces.Trace Float])
      partition = L' done step begin
      begin = ([], [])
      step :: ([a], [a]) -> (Word8, a) -> ([a], [a])
      step (p0, p1) (c, x) =
        if (c == 0)
        then (p0 ++ [x], p1)
        else (p0, p1 ++ [x])
      done = id

  -- compute the key hypothesis
  key <- importKey keyFile
  texts <- take nbTraces <$> importTexts textFile
  -- MAYBE use conduit to interleave traces loading with computations
  ts <- replicateM nbTraces $ loadfun tmin tmax

  -- FIXME.  the version commented below does not work. the bit indices are messed up.  i.e. bit 0 ~> 24, bit 8 ~> 16, etc.
  -- let hyps = force [ bitPosSt bit $ firstSBOX key t | t <- texts ] `using` parList rseq

  let targetByte = targetBit `div` 8
      k = getByte targetByte $ toAesText key
      getBit :: (Bits a, Num a) => Int -> a -> a
      getBit b w = w `shiftR` b .&. 0x1
      hyps = force (map (getBit $ targetBit `rem` 8) (fstSBOX' targetByte k texts)) `using` parList rseq

  let (pop0, pop1) = flip run partition $ zip hyps ts

  let tvals = flip run F.ttestU $ zip pop0 pop1

  -- plot t-test results
  -- ----------------
  let plotOpts = PlotTTest traceDir nbTraces tmin tmax targetBit
  plotTTest plotOpts tvals
  return ()

data PlotTTest = PlotTTest
  { plotDir  :: FilePath
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  , bitNb    :: Int
  }

-- | TODO
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
