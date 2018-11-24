{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPA
  ( cpa
  , CPAOptions(..)

  -- CLI
  , cmdCPAParser
  ) where

import           Conduit
import           Control.Concurrent.Async
import           Control.DeepSeq                        (NFData)
import           Control.Parallel.Strategies            hiding ((.|))
import           Data.Bits                              (popCount)
import           Data.Maybe                             (fromMaybe)
import qualified Data.Text                              as T
import qualified Data.Vector.Unboxed                    as U
import           Formatting
import           GHC.Conc                               (numCapabilities)
import           GHC.Generics                           (Generic)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative
import           System.FilePath.Posix                  (takeDirectory, (</>))
import           Text.Printf                            (printf)

import           Aes
import           Aes.Hypothesis
import           AesImport
import           CLI.Internal
import qualified Traces                                 as Traces

default (T.Text)


-- | CPA analysis
cpa :: CPAOptions -> IO ()
cpa CPAOptions{..} = do

  -- importing data traces
  (loadfun, tmax') <- Traces.importTraces traces

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
  key <- case keyFile of
    Nothing -> return $ stringImport  "0X01 0X23 0X45 0X67 0X89 0XAB 0XCD 0XEF 0X12 0X34 0X56 0X78 0X9A 0XBC 0XDE 0XF0"
    Just f  -> importKey f

  -- List of texts.  Our input file is a rather small file, so it may
  -- no be necessary to load it in the conduit stream.
  --
  -- TODO load texts in the conduit stream.
  texts <- take nbTraces <$> importTexts textFile :: IO [Plaintext]

  -- CPA analysis
  let buildCPATraces :: MonadIO m => ConduitT () CPATrace m ()
      buildCPATraces = getZipSource
                       $ CPATrace
                       <$> ZipSource loadTraces
                       <*> ZipSource (yieldMany $ map hypothesis texts)

      loadTraces :: MonadIO m => ConduitT () (Traces.Trace Float) m ()
      loadTraces = repeatMC (liftIO $ loadfun tmin tmax)

      hypothesis :: Plaintext -> [Float]
      hypothesis t = [ fromIntegral . popCount
                       -- ^ Hamming Weight
                       $ fstSBOX'' byteOpt k t | k <- [0..255]
                       -- ^ output of the first SBOX
                     ]

  corrs <- runConduit
           $ buildCPATraces
           .| takeC nbTraces
           .| pearson


  -- CPA analysis
  let (cs, cmaxs) = unzip corrs
      abscs = map (U.map abs) cs
      maxs = map U.maximum abscs

  fprint ("Max correlation value: " % float % " \n") $ maximum maxs
  fprint ("   found for key byte #" % float % " \n") $ U.maxIndex $ U.fromList maxs

  -- plot CPA results
  -- ----------------
  let plotOpts = PlotCPA traceDir byteOpt nbTraces tmin tmax
  let secret = fromIntegral $ getByte byteOpt $ toAesText key
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

pearson :: Monad m => ConduitT CPATrace Void m [(Traces.Trace Float, Traces.Trace Float)]
pearson = closeState <$> foldlC foldState emptyState

type PState = P (U.Vector Float) Float
data P a b =
  P !Int  -- index of the current iteration
    !a    -- mean of x (vector)
    !b    -- mean of y (scalar)
    !a    -- variance of x
    !a    -- covariance(x,y)
    !b    -- variance of y
    !a    -- history of max values of Pearson's correlation coefficient
  deriving (Eq, Show, Generic, NFData)

data CPATrace = CPATrace
                !(U.Vector Float)  -- the trace
                ![Float]  -- a list of hypothesis values

-- TODO use U.Vector.
-- emptyState ::  U.Vector ( PState (U.Vector Float) )
emptyState :: [PState]
emptyState = replicate 256 (P 0 u 0.0 u u 0.0 u)
  where u = U.empty :: U.Vector Float
-- emptyState = zipWith const (repeat $ P 0 U.empty U.empty U.empty U.empty U.empty U.empty) [0::Int ..255]

foldState :: [PState] -> CPATrace -> [PState]
foldState states (CPATrace trace hyps) =
  (zipWith (foldStep trace) states hyps) `using` (parListChunk numCapabilities) rdeepseq
{-# INLINABLE foldState #-}
foldStep :: (U.Vector Float) -> PState -> Float -> PState
foldStep t (P 0 _ _ _ _ _ _) h = P 1 t h sxx' sxy' syy' max'
  where
    zeros = U.map (const 0) t -- Assuming the two vectors have the same size
    sxx' = zeros -- sxx + devx*(x - xbar') = 0 + x * (x - x)
    sxy' = zeros
    syy' = 0.0
    max' = U.fromList [1] -- all correlation values are supposed to be '1' at the beginning.
foldStep t (P n xbar ybar sxx sxy syy m) h = P n' xbar' ybar' sxx' sxy' syy' max'
  where
    n' = n + 1
    devx = U.zipWith (-) t xbar
    devy = h - ybar
    xbar' = U.zipWith (+) xbar $ U.map (/ fromIntegral n') devx
    ybar' = ybar + devy / fromIntegral n'
    sxx' = U.zipWith (+) sxx $ U.zipWith (*) devx $ U.zipWith (-) t xbar'
    sxy' = U.zipWith (+) sxy $ U.map (* (h - ybar')) devx
    syy' = syy + devy * (h - ybar')
    c = U.zipWith (/) sxy $ U.map (\q -> sqrt (q * syy)) sxx
    max' = m `U.snoc` U.maximum c
{-# INLINABLE foldStep #-}

closeState :: [PState] -> [(Traces.Trace Float, Traces.Trace Float)]
closeState = map step
  where
    step :: PState -> (Traces.Trace Float, Traces.Trace Float)
    step (P _ _ _ sxx sxy syy m) =
      (U.zipWith (/) sxy $ U.map (\x -> sqrt (x * syy)) sxx, m)
{-# INLINABLE closeState #-}

data PlotCPA = PlotCPA
  { plotDir  :: FilePath
  , byte     :: Int
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  }

-- | plot Pearson's correlation.  Computation for n traces.
plotCPAT :: PlotCPA -> CorrelationSKey Float -> CorrelationHyps Float -> IO ()
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
plotCPAD :: PlotCPA -> CorrelationSKey Float -> CorrelationHyps Float -> IO ()
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


-- * CLI options

data CPAOptions = CPAOptions
  { traces   :: !TraceData
  , tmin     :: !Int          -- ^ the number of the first sample used
  , mtmax    :: !(Maybe Int)  -- ^ the number of the latest sample used
  , textFile :: !FilePath
  , keyFile  :: !(Maybe FilePath)
  , nbTraces :: !Int          -- ^ number of traces used for the CPA analysis
  , byteOpt  :: !Int
  } deriving (Show)

cmdCPAParser :: Parser CPAOptions
cmdCPAParser =
  CPAOptions
  <$> parseTraces
  <*> parseTmin
  <*> parseTmax
  <*> parseTextFile
  <*> optional parseKeyFile
  <*> parseNbTraces 512
  <*> option (fromInteger <$> auto)
  ( long "byte" <> short 'b'
    <> metavar "BYTE"
    <> help "Number of the key byte to attack [default: 0]"
    <> value 0
  )


-- * misc utils

-- | Delete element at index 'n' from the list.
--   This function is not safe if index 'n' is greater than the length of the input list.
deleteAt :: Int -> [a] -> [a]
deleteAt n xs = let (ys, zs) = splitAt n xs
                in  ys ++ tail zs

-- MAYBE. add performance tests with criterion.  bench the performance of this implementation.
-- MAYBE. replace lists with V.Vector, e.g. [PState] -> V.Vector PState
