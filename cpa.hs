{-# LANGUAGE OverloadedStrings #-}

-- import for Traces
import           Data.IORef
import           Data.List.Split
import           System.FilePath.Posix                  ((</>))

-- CPA
import           Prelude                                hiding (print)

import           Control.Monad
import           Data.List                              (delete)
import           Data.Text.Format
import qualified Data.Vector.Unboxed                    as U
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Statistics.Sample                      as S
import           Text.Printf           (printf)


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
  let tracesDir = "../traces"
  let byteNb = 0
  let corrNb = 256 :: Int -- compute the 256 possible key hypothesis

  -- the value of the secret key
  let key = stringImport  "0X01 0X23 0X45 0X67 0X89 0XAB 0XCD 0XEF 0X12 0X34 0X56 0X78 0X9A 0XBC 0XDE 0XF0" :: Key

  let keyrange = [0..(corrNb - 1)]
      secretByte = fromIntegral $ getByte byteNb $ toAesText key
      -- Do not compute the correlation twice for the true key
      -- hypothesis.  Furthermore, we move the true key hypothesis at
      -- the end of our list because of constraints on the way the
      -- display is handled; see functions makePicture and
      -- handleEvent.
      dontcomputetwicethetruekey = delete secretByte keyrange
      keyHyps = fromIntegral <$> (dontcomputetwicethetruekey ++ [secretByte])
      nbCorrTracesButKey = length dontcomputetwicethetruekey

  let keyHypothesisAS = keyHyps


  -- calcul sur toutes les traces, pour une seule hypothèse de clé seulement, sur l'octet 0
  -- import des traces
  h <- tracesInit tracesDir
  ts <- map trace <$> forM [(1::Int)..200] (\_ -> tracesLoad h) :: IO [U.Vector Float]
  tracesClose h

  -- calcul des hypothèses de clé
  -- TODO seulement sur la première clé pour l'instant
  let textfile = tracesDir </> "plaintexts.txt"
  texts <- importTexts textfile :: IO [Plaintext]
  let hyps = map (fromIntegral . head . fstSBOX 0 [1]) texts

  -- calcul des correlations
  let cs =  flip run pearsonUx $ zip ts hyps
      abscs = U.map abs cs

  print "Max correlation value (abs) : {}  \n"     [U.maximum abscs]
  print "Max correlation found for sample #{}  \n" [U.maxIndex abscs]

  -- tracé des courbes CPA
  toFile def "example1_big.png" $ do
    layout_title .= "Pearson's correlation coefficient"
    setColors [opaque blue, opaque red]
    plot (line "correlation" $ [zip [(1::Float)..10000] $ U.toList cs])

test :: (Num a, U.Unbox a) => (a -> a -> a) -> [U.Vector a] -> [U.Vector a] -> [[a]]
test f ts ks = [ [ U.sum $ U.zipWith f t k | t <- ts] | k <- ks]
  -- do
  --   k <- ks
  --   do
  --     t <- ts
  --     U.sum $ U.zipWith f t k


-- stepCorr :: TraceHandle -> Byte -> [Word8] -> Plaintext -> IO () -- TODO return type
-- stepCorr trace byteNb kh txt = do
--   -- the key hypothesis
--   let hyps = fromIntegral <$> fstSBOX byteNb kh txt

--   -- compute the Pearson correlation, and return the new correlation state
--   let s' = mapP (pearson <$> hyps <*> (float2Double <$> trace)) pstate

--   -- extract the correlation values
--   let rs = map (double2Float . fst) s'

--   -- split into several lists, one for each key hypothesis
--   let ls = chunksOf tsize $ zip ( fmap ( fromIntegral      -- convert to floats
--                                          . (+ tmin)        -- add an offset of tmin
--                                          . flip rem tsize  -- restart at index 0 every tsize
--                                        ) [0..]             -- a list of indices, starting a 0
--                                 ) rs
--   -- print the max correlation value
--   print $ maximum $ map (abs . snd) $ last ls

--   -- correlation curves for all the key hypothesis
--   --   the last Line of the list is associated to the true key.
--   let corrs = Line <$> ls

--   return s'



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
  t <- readIORef n >>= tracesLoad' h
  modifyIORef' n (+1)
  return t

-- | Load a specific trace.
tracesLoad' :: (Read a, U.Unbox a) => TraceHandle -> Int -> IO (Trace a)
tracesLoad' (TraceHandle d _) i = do
  -- s <- readFile $ d </> (show $ format "trace_{}.txt" [ left 9 '0' i ])
  s <- readFile $ d </> printf "trace_%09d.txt" i
  return $ Trace $ U.fromList $ map read $ takeWhile (not . null) $ splitOn " " s

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
