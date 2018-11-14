{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


-- CPA
import           Prelude

import           Control.Concurrent.Async
import           Control.DeepSeq                        (force)
import           Control.Parallel.Strategies
import           Data.Bits                              (popCount)
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import qualified Data.Text                              as T
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Version                           as V (showVersion)
import           Formatting
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative
import           Paths_haskell_aes                      (version)
import           System.FilePath.Posix                  (takeDirectory, (</>))
import           Text.Printf                            (printf)

import           Aes
import           Aes.Hypothesis
import           AesImport
import           Folds
import qualified Traces.Raw                             as Traces

default (T.Text)

-- TODO t-test
-- TODO nouvelle méthode Pearson -> scarlet
-- TODO t-test -> scarlet
-- TODO optimiser le chargement des traces: format binaire, un seul fichier ou plusieurs ? (pour l'instant, c'est au moins la moitié du temps de calcul !!)

main :: IO ()
main = do
  opts <- execParser optInfo

  -- importing data traces
  (h, tmax') <- case tracesOpt opts of
    TraceRawFile f -> do
      h <- Traces.init f
      return (h, Traces.size h)
    -- TODO restore format for traces in text format
    TracesDir _ -> error "TODO -- unsupported trace format"

  let traceDir = case tracesOpt opts of
        TraceRawFile f -> takeDirectory f
        TracesDir d    -> d

  let tmin = tminOpt opts
      tmax = fromMaybe tmax' (tmaxOpt opts)
      nbtraces = nbTracesOpt opts

  -- print some user settings
  fprint "Settings: \n"
  let fter :: Format r (T.Text -> Int -> r)
      fter = right 20 ' ' % left 10 ' ' % "\n"
  fprint fter "number of traces: " nbtraces
  fprint fter "tmin: " tmin
  fprint fter "tmax: " tmax
  fprint "\n"

  -- calcul des hypothèses de clé
  keys <- case keyFile opts of
            Nothing -> return [stringImport  "0X01 0X23 0X45 0X67 0X89 0XAB 0XCD 0XEF 0X12 0X34 0X56 0X78 0X9A 0XBC 0XDE 0XF0"]
            Just f  -> importTexts f
  let key = case keys of
        [k] -> k :: Key
        _   -> error "Error.  Found more than one key value in the keyFile"

  texts <- importTexts (textFile opts) :: IO [Plaintext]

  -- the key hypothesis
  let keyHyps = [0..255]
      secret = fromIntegral $ getByte (byteOpt opts) $ toAesText key
      txts = take nbtraces texts
      hyps' = [ popCount <$> fstSBOX' (byteOpt opts) k txts | k <- keyHyps]
      hyps = [map fromIntegral h | h <- hyps']

  -- CPA analysis
  let traces = Traces.load' h tmin tmax
  let cs' = force ((map (\h -> flip run pearsonUx $ zip traces h) hyps) `using` parList rseq)
  let (cs, cmaxs) = unzip cs'
      abscs = [ U.map abs c | c <- cs ]
      maxs = U.fromList [ U.maximum x | x <- abscs ]

  fprint ("Max correlation value: " % float % " \n") $ U.maximum maxs
  fprint ("   found for key byte #" % float % " \n") $ U.maxIndex maxs

  -- tracé des courbes CPA
  -- ---------------------

  let plotOpts = PlotCPA traceDir (byteOpt opts) nbtraces tmin tmax

  -- courbe (correlation, #sample); projection sur chaque instant d'échantillonnage pour le nombre max d'itérations
  p0 <- async $ plotCPAT plotOpts (CorrelationSKey $ cs !! secret) (CorrelationHyps $ deleteAt secret cs)

  -- courbe (correlation, #trace); projection sur le nombre de traces
  p1 <- async $ plotCPAD plotOpts (CorrelationSKey $ cmaxs !! secret) (CorrelationHyps $ deleteAt secret cmaxs)

  wait p0
  wait p1


newtype CorrelationHyps a = CorrelationHyps [U.Vector a]
newtype CorrelationSKey a = CorrelationSKey (U.Vector a)

data PlotCPA = PlotCPA
  { plotDir  :: FilePath
  , byte     :: Int
  , plotSize :: Int
  , plotTmin :: Int
  , plotTmax :: Int
  }

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

-- it's time to introduce the Reader Monad!
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



-- * CLI options
data AppOptions = AppOptions
  { -- cmd        :: !Command
    tracesOpt   :: !TraceData
  , textFile    :: !FilePath
  , keyFile     :: !(Maybe FilePath)
  , byteOpt     :: !Int
  , nbTracesOpt :: Int        -- ^ number of traces used for the CPA analysis
  , tminOpt     :: Int        -- ^ the number of the first sample used
  , tmaxOpt     :: Maybe Int  -- ^ the number of the latest sample used
  } deriving (Show)

data TraceData = TracesDir FilePath
               | TraceRawFile FilePath
               deriving (Show, Eq)

-- | read program options
optParser :: Parser AppOptions
optParser = AppOptions
  <$> (( TracesDir <$> strOption
         ( metavar "TRACES_DIR"
           <> help "Location of the directory with traces files, in textual format."
           <> long "traces-dir"
           <> short 'd'
         )
       )
       <|> ( TraceRawFile <$> strOption
             ( metavar "TRACE_RAWFILE"
               <> help "Location of the trace files, in raw format."
               <> long "trace-rawfile"
               <> short 'f'
             )
           )
      )
  <*> strOption ( long "textfile" <> short 't'
                  <> metavar "TEXTFILE"
                  <> help "Location of the plaintexts file  [default: TRACES_DIR/plaintexts.txt]"
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
  ( long "nbtraces" <> short 'n'
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
