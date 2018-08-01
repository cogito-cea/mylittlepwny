{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


-- import for Traces
import           Data.List.Split
import           System.FilePath.Posix                  ((</>))

-- CPA
import           Prelude                                hiding (print)

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.DeepSeq                        (force)
import           Control.Exception                      (bracket)
import           Control.Exception                      (evaluate)
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Bits                              (popCount)
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import           Data.Text.Format
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Version                           as V (showVersion)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative
import           Paths_haskell_aes                      (version)
import           Text.Printf                            ( printf)


import           Aes
import           Aes.Hypothesis
import           AesImport
import           Folds

-- TODO data-parallel version
-- TODO t-test

main :: IO ()
main = do
  opts <- execParser optInfo

  -- calcul sur toutes les traces, pour une seule hypothèse de clé seulement, sur l'octet 0
  -- calcul de la longueur des traces
  tmax' <- case tmaxOpt opts of
    Just t -> return t
    Nothing -> do
      t <- bracket (tracesInit $ tracesOpt opts) tracesClose (\h -> tracesLoad h 0) :: IO (Trace Int)
      return $ U.length $ trace t

  -- import des traces
  h <- tracesInit $ tracesOpt opts
  let tmin' = tminOpt opts
      tlen  = tmax' - tmin'
      nsize' = nsizeOpt opts
  traces <- map trace <$> forConcurrently [(0::Int)..(nsize'-1)] (\i -> tracesLoad' h i tmin' tlen) :: IO [U.Vector Float]
  tracesClose h

  -- calcul des hypothèses de clé
  keys <- case keyFile opts of
            Nothing -> return [stringImport  "0X01 0X23 0X45 0X67 0X89 0XAB 0XCD 0XEF 0X12 0X34 0X56 0X78 0X9A 0XBC 0XDE 0XF0"]
            Just f  -> importTexts f
  let key = case keys of
        [k] -> k :: Key
        _   -> error "Error.  Found more than one key value in the keyFile"

  let textfile = fromMaybe ((tracesOpt opts) </> "plaintexts.txt") (textFile opts)
  texts <- importTexts textfile :: IO [Plaintext]

  -- the key hypothesis
  let keyHyps = [0..255]
      secret = fromIntegral $ getByte (byteOpt opts) $ toAesText key
      txts = take nsize' texts
      hyps' = [ popCount <$> fstSBOX' (byteOpt opts) k txts | k <- keyHyps]
      hyps = [map fromIntegral h | h <- hyps']

  -- calcul des correlations
  let cs' = force
            $ ((map (\h -> flip run pearsonUx $ zip traces h) hyps) `using` parList rseq)
  let (cs, cmaxs) = unzip cs'
      abscs = [ U.map abs c | c <- cs ]
      maxs = U.fromList [ U.maximum x | x <- abscs ]

  print "Max correlation value: {} \n" [U.maximum maxs]
  print "   found for key byte #{} \n" [U.maxIndex maxs]

  -- tracé des courbes CPA
  -- ---------------------

  let plotOpts = PlotCPA (tracesOpt opts) (byteOpt opts) nsize' tmin' tmax'

  -- courbe (correlation, #sample); projection sur chaque instant d'échantillonnage pour le nombre max d'itérations
  p0 <- async $ plotCPAT plotOpts (CorrelationSKey $ cs !! secret) (CorrelationHyps $ deleteAt secret cs)

  -- courbe (correlation, #trace); projection sur le nombre de traces
  p1 <- async $ plotCPAD plotOpts (CorrelationSKey $ cmaxs !! secret) (CorrelationHyps $ deleteAt secret cmaxs)

  wait p0
  wait p1

newtype CorrelationHyps a = CorrelationHyps [U.Vector a]
newtype CorrelationSKey a = CorrelationSKey (U.Vector a)

data PlotCPA = PlotCPA
  { tracesDir :: FilePath
  , byte      :: Int
  , nsize     :: Int
  , tmin      :: Int
  , tmax      :: Int
  }

plotCPAT :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotCPA -> CorrelationSKey a -> CorrelationHyps a -> IO ()
plotCPAT PlotCPA{..} (CorrelationSKey dsecret) (CorrelationHyps dhyps) = do
  let graphFile = tracesDir
                  </> printf "CPA-T byte:%d n:%d tmin:%05d tmax:%05d.png" byte nsize tmin tmax
  print "\nRendering the CPA plot in: {}\n" [graphFile]
  toFile def graphFile $ do
    layout_title .= printf "Pearson's correlation. Computation for %d traces." nsize
    setColors [ opaque grey, opaque black]
    let abscissa = [(fromIntegral tmin)..] :: [Float]
    plot $ line "Wrong key hypotheses" $ [ zip abscissa $ U.toList c | c <- dhyps ]
    plot $ line "Secret key" $ [ zip abscissa $ U.toList $ dsecret ]

-- it's time to introduce the Reader Monad!
plotCPAD :: (Num a, PlotValue a, RealFloat a, Show a, U.Unbox a)
  => PlotCPA -> CorrelationSKey a -> CorrelationHyps a -> IO ()
plotCPAD PlotCPA{..} (CorrelationSKey dsecret) (CorrelationHyps dhyps) = do
  let fd = tracesDir
                  </> printf "CPA-D byte:%d n:%d tmin:%05d tmax:%05d.png" byte nsize tmin tmax
  print "\nRendering the CPA plot in: {}\n" [fd]
  toFile def fd $ do
    layout_y_axis . laxis_generate .= scaledAxis def (0, 1)
    layout_title .=
      printf "Pearson's correlation. Projection over the number of traces used. t ∈ [%d; %d]" tmin tmax
    setColors [ opaque grey, opaque black]
    let abscissa = [0..] :: [Float]
    plot $ line "Wrong key hypotheses" $ [ zip abscissa $ U.toList c | c <- dhyps ]
    plot $ line "Secret key" $ [ zip abscissa $ U.toList dsecret ]


-- * Traces
newtype Trace a = Trace { trace :: U.Vector a
                        } deriving (Show)

data TraceHandle = TraceHandle
  { tracedir :: FilePath
  -- , counter  :: IORef Int -- Still not happy with the API for loading functions...
  }

tracesInit :: FilePath -> IO TraceHandle
tracesInit d = return $ TraceHandle d

-- | Load a trace.
tracesLoad :: (Read a, U.Unbox a) => TraceHandle -> Int -> IO (Trace a)
tracesLoad (TraceHandle d) i = do
  s <- readFile $ d </> printf "trace_%09d.txt" i
  return $! Trace $ U.fromList $ map read $ takeWhile (not . null) $ splitOn " " s

-- | Load a new trace, filter samples out of the window of interest
tracesLoad' :: (Read a, U.Unbox a) => TraceHandle
                                          -> Int  -- ^ the trace number
                                          -> Int  -- ^ tmin. index of the first sample
                                          -> Int  -- ^ len.  size of the sample window
                                          -> IO (Trace a)
tracesLoad' (TraceHandle d) i m l = do
  s <- readFile $ d </> printf "trace_%09d.txt" i
  return $! Trace $ U.fromList
                 $ take l $ drop m
                 $ map read $ takeWhile (not . null) $ splitOn " " s

tracesClose :: TraceHandle -> IO ()
tracesClose _ = return ()


-- * CLI options
data AppOptions = AppOptions
  { -- cmd        :: !Command
    tracesOpt :: FilePath
  , textFile  :: !(Maybe FilePath)
  , keyFile   :: !(Maybe FilePath)
  , byteOpt   :: !Int
  , nsizeOpt  :: Int        -- ^ number of traces used for the CPA analysis
  , tminOpt   :: Int        -- ^ the number of the first sample used
  , tmaxOpt   :: Maybe Int  -- ^ the number of the latest sample used
  } deriving (Show)

-- | read program options
optParser :: Parser AppOptions
optParser = AppOptions
  <$> strArgument
  ( metavar "TRACES_DIR"
    <> help "Location of the traces files"
  )
  <*> optional ( strOption
                 ( long "textfile" <> short 't'
                   <> metavar "TEXTFILE"
                   <> help "Location of the plaintexts file  [default: TRACES_DIR/plaintexts.txt]"
                 )
               )
  <*> optional ( strOption
                 ( long "keyfile" <> short 'k'
                   <> metavar "KEYFILE"
                   <> help "Location of the key file"
                 )
               )
  <*> option (fromInteger <$> auto)
  ( long "byte" <> short 'b'
    <> metavar "BYTE"
    <> help "Number of the key byte to attack [default: 0]"
    <> value 0
  )
  <*> option (fromInteger <$> auto)
  ( long "nsize" <> short 'n'
    <> metavar "NSIWE"
    <> help "Number of traces used for the CPA analysis [default: 512]"
    <> value 512
  )
  <*> option (fromInteger <$> auto)
  ( long "tmin"
    <> metavar "TMIN"
    <> help "Sample number for the start of the observation window [default: 0]."
    <> value 0
  )
  <*> optional ( option (fromInteger <$> auto)
                 ( long "tmax"
                   <> metavar "TMAX"
                   <> help "Sample number for the end of the observation window [default: full trace length]."
                 )
               )

optInfo :: ParserInfo AppOptions
optInfo = info
          ( helper <*> versionOption <*> optParser )
          ( fullDesc
            <> progDesc "CPA from on-disk traces"
          )
  where
    versionOption = infoOption showVersion (long "version" <> help "Show version")

-- | show version info
showVersion :: String
showVersion = V.showVersion version

-- * misc utils
-- | Delete element at index 'n' from the list.
--   This function is not safe if index 'n' is greater than the length of the input list.
deleteAt :: Int -> [a] -> [a]
deleteAt n xs = let (ys, zs) = splitAt n xs
                in  ys ++ tail zs
