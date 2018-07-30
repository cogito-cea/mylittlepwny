{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- import for Traces
import           Data.IORef
import           Data.List.Split
import           System.FilePath.Posix                  ((</>))

-- CPA
import           Prelude                                hiding (print)

import           Control.Exception                      (bracket)
import           Control.Monad
import           Data.List                              (delete)
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import           Data.Text.Format
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Version                           as V (showVersion)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative
import           Paths_haskell_aes                      (version)
import qualified Statistics.Sample                      as S
import           Text.Printf                            (printf)


import           Aes
import           Aes.Hypothesis
import           AesImport
import           Folds

-- statisticsSample :: StatsResults
-- statisticsSample =
--   ( SS.mean $ U.fromList x
--   , SS.mean $ U.fromList y
--   , SS.stdDev $ U.fromList x
--   , SS.stdDev $ U.fromList y
--   , SS.correlation $ U.fromList $ zip x y
--   )

main :: IO ()
main = do
  opts <- execParser optInfo

  -- calcul sur toutes les traces, pour une seule hypothèse de clé seulement, sur l'octet 0
  -- calcul de la longueur des traces
  tmax' <- case tmax opts of
    Just t -> return t
    Nothing -> do
      t <- bracket (tracesInit $ tracesDir opts) tracesClose tracesLoad :: IO (Trace Int)
      return $ U.length $ trace t

  -- import des traces
  h <- tracesInit $ tracesDir opts
  let tmin' = tmin opts
      len   = tmax' - tmin'
      nsize' = nsize opts
  traces <- map trace <$> forM [(0::Int)..(nsize'-1)] (\i -> tracesLoadIndexed' h i tmin' len) :: IO [U.Vector Float]
  tracesClose h

  -- calcul des hypothèses de clé
  -- TODO seulement sur la première clé pour l'instant
  keys <- case keyFile opts of
            Nothing -> return [stringImport  "0X01 0X23 0X45 0X67 0X89 0XAB 0XCD 0XEF 0X12 0X34 0X56 0X78 0X9A 0XBC 0XDE 0XF0"]
            Just f  -> importTexts f
  let key = case keys of
        [k] -> k :: Key
        _   -> error "Error.  Found more than one key value in the keyFile"

  let textfile = fromMaybe ((tracesDir opts) </> "plaintexts.txt") (textFile opts)
  texts <- importTexts textfile :: IO [Plaintext]

  -- the key hypothesis

  let keyrange = [0..255]
      secretByte = fromIntegral $ getByte (byte opts) $ toAesText key
      -- Do not compute the correlation twice for the true key
      -- hypothesis.  Furthermore, we move the true key hypothesis at
      -- the end of our list because of constraints on the way the
      -- display is handled; see functions makePicture and
      -- handleEvent.
      others = delete secretByte keyrange

      txts = take nsize' texts
      hyps' = [ fstSBOX' (byte opts) k txts | k <- (secretByte : others)]
      hyps = [map fromIntegral h | h <- hyps']
      -- hyps :: [[Float]]
  print "len hyps': {}\n" [length hyps']
  print "len hyps: {}\n" [length hyps]
  print "len head hyps: {}\n" [length $ head hyps]

  -- calcul des correlations
  let cs = [ flip run pearsonUx $ zip traces h | h <- hyps ]
  --     abscs = [ U.map abs c | c <- cs ]

  -- print "Max correlation value (abs) : {} {} \n"     [U.maximum a | a <- abscs]
  -- print "Max correlation found for sample #{} {}  \n" [U.maxIndex a | a <- abscs]

  -- tracé des courbes CPA
  let graphFile = printf "CPA byte:%d n:%d tmin:%05d tmax:%05d.png" (byte opts) nsize' tmin' tmax'
  print "\nRendering the CPA plot in: {}\n" [graphFile]
  let dataplot = [ zip [(1::Float)..10000] $ U.toList c | c <- cs ]
  toFile def graphFile $ do
    layout_title .= "Pearson's correlation coefficient"
    setColors [ opaque grey, opaque black]
    plot (line "correlation" $ tail dataplot)
    plot (line "correlation" $ [head dataplot])

test :: (Num a, U.Unbox a) => (a -> a -> a) -> [U.Vector a] -> [U.Vector a] -> [[a]]
test f ts ks = [ [ U.sum $ U.zipWith f t k | t <- ts] | k <- ks]
  -- do
  --   k <- ks
  --   do
  --     t <- ts
  --     U.sum $ U.zipWith f t k



-- * Traces
newtype Trace a = Trace { trace :: U.Vector a
                        } deriving (Show)

data TraceHandle = TraceHandle
  { tracedir :: FilePath
  , counter  :: IORef Int
  }

tracesInit :: FilePath -> IO TraceHandle
tracesInit d = TraceHandle d <$> newIORef 0

-- | Load a new trace.
tracesLoad :: (Read a, U.Unbox a) => TraceHandle -> IO (Trace a)
tracesLoad h@(TraceHandle _ n) = do
  t <- readIORef n >>= tracesLoadIndexed h
  modifyIORef' n (+1)
  return t

-- | Load a new trace, filter samples out of the window of interest
tracesLoad' :: (Read a, U.Unbox a) => TraceHandle  -- ^ the trace handle
                                   -> Int          -- ^ tmin.  index of the first sample
                                   -> Int          -- ^ tmax.  size of the sample window
                                   -> IO (Trace a) -- ^ the loaded trace
tracesLoad' h@(TraceHandle _ n) tmin len = do
  x <- readIORef n
  t <- tracesLoadIndexed' h x tmin len
  modifyIORef' n (+1)
  return t

-- | Load a specific trace.
tracesLoadIndexed :: (Read a, U.Unbox a) => TraceHandle -> Int -> IO (Trace a)
tracesLoadIndexed (TraceHandle d _) i = do
  -- s <- readFile $ d </> (show $ format "trace_{}.txt" [ left 9 '0' i ])
  s <- readFile $ d </> printf "trace_%09d.txt" i
  return $ Trace $ U.fromList $ map read $ takeWhile (not . null) $ splitOn " " s
tracesLoadIndexed' :: (Read a, U.Unbox a) => TraceHandle
                                          -> Int
                                          -> Int
                                          -> Int
                                          -> IO (Trace a)
tracesLoadIndexed' (TraceHandle d _) i tmin len = do
  s <- readFile $ d </> printf "trace_%09d.txt" i
  return $ Trace $ U.fromList
                 $ take len $ drop tmin
                 $ map read $ takeWhile (not . null) $ splitOn " " s

tracesClose :: TraceHandle -> IO ()
tracesClose _ = return ()


{-|
>>> :t flip run pearsonUx
flip run pearsonUx
  :: (Data.Vector.Unboxed.Base.Unbox a, Floating a, Foldable t) =>
     t (Data.Vector.Unboxed.Base.Vector a, a)
     -> Data.Vector.Unboxed.Base.Vector a
>>> import qualified Data.Vector.Unboxed as U
>>> let ks = [1,1,2,3]
>>> let xs = [ U.fromList [0,1,0,0,0], U.fromList [1,1,1,1,1], U.fromList [0,2,1,1,1], U.fromList [0,3,0,0.5,0]]
>>> flip run pearsonUx $ zip xs ks
[-0.2842676218074806,0.8771649338308385,-5.504818825631798e-2,0.34510571758123426,-5.504818825631798e-2]



-- loading traces

>>> ts <- take 100 . repeat <$> tracesLoad h :: IO ([Trace Float])
>>> :t ts
ts :: [Trace Float]
>>> length ts
100


>>> let textfile = tracesDir </> "plaintexts.txt"
>>> texts <- importTexts textfile :: IO [Plaintext]

>>> let hyps = map (head . fstSBOX 0 [0]) texts


-}


-- * CLI options
data AppOptions = AppOptions
  { -- cmd        :: !Command
    tracesDir :: FilePath
  , textFile  :: !(Maybe FilePath)
  , keyFile   :: !(Maybe FilePath)
  , byte      :: !Int
  , nsize     :: Int        -- ^ number of traces used for the CPA analysis
  , tmin      :: Int        -- ^ the number of the first sample used
  , tmax      :: Maybe Int  -- ^ the number of the latest sample used
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
