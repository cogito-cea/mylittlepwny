{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module CPA
  ( cpa
  ) where

import           Control.Concurrent.Async
import           Control.DeepSeq                        (force)
import           Control.Monad                          (replicateM)
import           Control.Parallel.Strategies
import           Data.Bits                              (popCount)
import           Data.Maybe                             (fromMaybe)
import qualified Data.Text                              as T
import qualified Data.Vector.Unboxed                    as U
import           Formatting
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.FilePath.Posix                  (takeDirectory, (</>))
import           Text.Printf                            (printf)

import           Aes
import           Aes.Hypothesis
import           AesImport
import           CLI
import           Folds
import qualified Traces.Raw                             as Traces

default (T.Text)


-- | CPA analysis
cpa :: CPAOptions -> IO ()
cpa CPAOptions{..} = do

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
      fter = right 20 ' ' % left 10 ' ' % "\n"
  fprint "CPA Settings: \n"
  fprint fter "number of traces: " nbTraces
  fprint fter "tmin: " tmin
  fprint fter "tmax: " tmax
  fprint "\n"

  -- compute the key hypothesis
  keys <- case keyFile of
            Nothing -> return [stringImport  "0X01 0X23 0X45 0X67 0X89 0XAB 0XCD 0XEF 0X12 0X34 0X56 0X78 0X9A 0XBC 0XDE 0XF0"]
            Just f  -> importTexts f
  let key = case keys of
        [k] -> k :: Key
        _   -> error "Error.  Found more than one key value in the keyFile"

  texts <- importTexts textFile :: IO [Plaintext]

  let keyHyps = [0..255]
      byte = byteOpt
      secret = fromIntegral $ getByte byte $ toAesText key
      txts = take nbTraces texts
      hyps' = [ popCount <$> fstSBOX' byte k txts | k <- keyHyps]
      hyps = [map fromIntegral h | h <- hyps']

  -- CPA analysis
  -- TODO fix workaround replicateM, use Conduit
  ts <- replicateM nbTraces $ Traces.load' h tmin tmax
  let cs' = force ((map (\h -> flip run pearsonUx $ zip ts h) hyps) `using` parList rseq)
  let (cs, cmaxs) = unzip cs'
      abscs = [ U.map abs c | c <- cs ]
      maxs = U.fromList [ U.maximum x | x <- abscs ]

  fprint ("Max correlation value: " % float % " \n") $ U.maximum maxs
  fprint ("   found for key byte #" % float % " \n") $ U.maxIndex maxs

  print $ length cmaxs
  -- TODO les vecteurs n'ont pas la bonne longueur -- manque un élément
  print $ U.length $ cmaxs !! secret
  print $ length cs

  -- plot CPA results
  -- ----------------
  let plotOpts = PlotCPA traceDir byte nbTraces tmin tmax
  concurrently_
    (
      -- courbe (correlation, #sample); projection sur chaque instant d'échantillonnage pour le nombre max d'itérations
      plotCPAT plotOpts (CorrelationSKey $ cs !! secret) (CorrelationHyps $ deleteAt secret cs)
    )
    (
      -- courbe (correlation, #trace); projection sur le nombre de traces
      plotCPAD plotOpts (CorrelationSKey $ cmaxs !! secret) (CorrelationHyps $ deleteAt secret cmaxs)
    )

-- | Correlation for all hypothesis but the secret key
newtype CorrelationHyps a = CorrelationHyps [Traces.Trace a]
-- | Correlation for the secret key
newtype CorrelationSKey a = CorrelationSKey (Traces.Trace a)

data PlotCPA = PlotCPA
  { plotDir  :: FilePath
  , byte     :: Int
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  }

-- | plot Pearson's correlation.  Computation for n traces.
plotCPAT :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotCPA -> CorrelationSKey a -> CorrelationHyps a -> IO ()
plotCPAT PlotCPA{..} (CorrelationSKey dsecret) (CorrelationHyps dhyps) = do
  let graphFile = plotDir
                  </> printf "CPA-T byte:%d n:%d tmin:%05d tmax:%05d.png" byte plotSize plotTmin plotTmax
  fprint ("\nRendering the CPA plot in: " % string % " \n") graphFile
  toFile def graphFile $ do
    layout_title .= printf "Pearson's correlation. Computation for %d traces." plotSize
    setColors [ opaque grey, opaque black]
    let abscissa = [(fromIntegral plotTmin)..] :: [Float]
    plot $ line "Wrong key hypotheses" $ [ zip abscissa $ U.toList c | c <- dhyps ]
    plot $ line "Secret key" $ [ zip abscissa $ U.toList $ dsecret ]

-- | plot Pearson's correlation. Projection over the number of traces used
--
-- MAYBE it's time to introduce the Reader Monad!
plotCPAD :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotCPA -> CorrelationSKey a -> CorrelationHyps a -> IO ()
plotCPAD PlotCPA{..} (CorrelationSKey dsecret) (CorrelationHyps dhyps) = do
  let fd = plotDir
           </> printf "CPA-D byte:%d n:%d tmin:%05d tmax:%05d.png" byte plotSize plotTmin plotTmax
  fprint ("\nRendering the CPA plot in: " % string % "\n") fd
  toFile def fd $ do
    layout_y_axis . laxis_generate .= scaledAxis def (0, 1)
    layout_title .=
      printf "Pearson's correlation. Projection over the number of traces used. t ∈ [%d; %d]" plotTmin plotTmax
    setColors [ opaque grey, opaque black]
    let abscissa = [0..] :: [Float]
    plot $ line "Wrong key hypotheses" $ [ zip abscissa $ U.toList c | c <- dhyps ]
    plot $ line "Secret key" $ [ zip abscissa $ U.toList dsecret ]


-- * misc utils
-- | Delete element at index 'n' from the list.
--   This function is not safe if index 'n' is greater than the length of the input list.
deleteAt :: Int -> [a] -> [a]
deleteAt n xs = let (ys, zs) = splitAt n xs
                in  ys ++ tail zs
