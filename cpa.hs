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
import           Text.Printf                            (printf)


import           Aes
import           Aes.Hypothesis
import           AesImport
import           Folds

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
  let keyHyps = [0..255]
      secret = fromIntegral $ getByte (byte opts) $ toAesText key
      txts = take nsize' texts
      hyps' = [ popCount <$> fstSBOX' (byte opts) k txts | k <- keyHyps]
      hyps = [map fromIntegral h | h <- hyps']

  -- calcul des correlations
  let cs = [ flip run pearsonUx $ zip traces h | h <- hyps ]
      abscs = [ U.map abs c | c <- cs ]
      maxs = U.fromList [ U.maximum x | x <- abscs ]

  print "Max correlation value: {} \n" [U.maximum maxs]
  print "   found for key byte #{} \n" [U.maxIndex maxs]

  -- tracé des courbes CPA
  let graphFile = tracesDir opts
                  </> printf "CPA byte:%d n:%d tmin:%05d tmax:%05d.png" (byte opts) nsize' tmin' tmax'
  print "\nRendering the CPA plot in: {}\n" [graphFile]
  let datahyps = [ zip [(1::Float)..10000] $ U.toList c | c <- deleteAt secret cs ]
  let datasecret = [ zip [(1::Float)..10000] $ U.toList $ cs !! secret]

  -- TODO fix axe abscisse si tmin != 0
  toFile def graphFile $ do
    layout_title .= "Pearson's correlation coefficient"
    setColors [ opaque grey, opaque black]
    plot $ line "correlation" $ datahyps
    plot $ line "correlation" $ datasecret


-- * Traces
newtype Trace a = Trace { trace :: U.Vector a
                        } deriving (Show)

data TraceHandle = TraceHandle
  { tracedir :: FilePath
  , counter  :: IORef Int -- TODO delete this
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
                                   -> Int          -- ^ len.  size of the sample window
                                   -> IO (Trace a) -- ^ the loaded trace
tracesLoad' h@(TraceHandle _ n) m l = do
  x <- readIORef n
  t <- tracesLoadIndexed' h x m l
  modifyIORef' n (+1)
  return t

-- | Load a specific trace.
tracesLoadIndexed :: (Read a, U.Unbox a) => TraceHandle -> Int -> IO (Trace a)
tracesLoadIndexed (TraceHandle d _) i = do
  -- s <- readFile $ d </> (show $ format "trace_{}.txt" [ left 9 '0' i ])
  s <- readFile $ d </> printf "trace_%09d.txt" i
  return $! Trace $ U.fromList $ map read $ takeWhile (not . null) $ splitOn " " s
tracesLoadIndexed' :: (Read a, U.Unbox a) => TraceHandle
                                          -> Int  -- ^ the trace number
                                          -> Int  -- ^ tmin. index of the first sample
                                          -> Int  -- ^ len.  size of the sample window
                                          -> IO (Trace a)
tracesLoadIndexed' (TraceHandle d _) i m l = do
  s <- readFile $ d </> printf "trace_%09d.txt" i
  return $! Trace $ U.fromList
                 $ take l $ drop m
                 $ map read $ takeWhile (not . null) $ splitOn " " s

tracesClose :: TraceHandle -> IO ()
tracesClose _ = return ()


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

-- * misc utils
-- | Delete element at index 'n' from the list.
--   This function is not safe if index 'n' is greater than the length of the input list.
deleteAt :: Int -> [a] -> [a]
deleteAt n xs = let (ys, zs) = splitAt n xs
                in  ys ++ tail zs
