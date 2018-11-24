{-|

Most of this module was inspired by Gabriel Gonzalez's foldl library,
   and by Ed Kmett's folds library: I needed the completeness of
   foldl, but with the flexibility of folds.  This re-implementation
   is based on folds, but re-implements the statistics functions
   provided by foldl, except for Pearson's correlation and for Welch's
   t-test.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS -Wno-orphans #-}

module Folds
  (
    -- * scalar folds
    genericLength
  , Folds.maximum
  , Folds.minimum
  , sum
  , mean
  , std
  , ttest
  , pearson

  -- * vector implementations, for Data.Vector.Unboxed
  , meanU
  , ttestU
  , pearsonUx
  , pearsonUU
  , pearsonUUx

  -- re-exports
  , run
  ) where


import           Control.Applicative (liftA2)
import           Data.Fold
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import           Prelude             hiding (sum)

-- L
-- A Moore Machine
-- L (r -> b) (r -> a -> r) r

-- data Fold a b
--   -- | @Fold @ @ step @ @ initial @ @ extract@
--   = forall x. Fold (x -> a -> x) x (x -> b)

data Pair a b = Pair !a !b
data Pair3 a b c = Pair3 !a !b !c

-- | Like 'length', except with a more general 'Num' return value
genericLength :: Num b => L' a b
genericLength = L' id (\n _ -> n + 1) 0
{-# INLINABLE genericLength #-}

maximum :: Ord a => L' a (Maybe a)
maximum = _L' max
{-# INLINABLE maximum #-}

minimum :: Ord a => L' a (Maybe a)
minimum = _L' min
{-# INLINABLE minimum #-}

_L' :: (a -> a -> a) -> L' a (Maybe a)
_L' step = L' done step_ begin
  where
    begin = Nothing'
    step_ Nothing'   y = Just' y
    step_ (Just' x) y  = Just' (step x y)
    done x = toLazy x
{-# INLINABLE _L' #-}

sum :: Num a => L' a a
sum = L' id (+) 0
{-# INLINABLE sum #-}

mean :: (Fractional a, Eq a) => L' a a
mean = L' done step begin
  where
    begin = Pair 0 0 -- Pair sum nb_items
    done (Pair x _) = x
    step (Pair _ 0) x = Pair x 1
    step (Pair m n) x = Pair (m + (x - m) / (n + 1)) (n + 1)
{-# INLINABLE mean #-}

-- | mean for Data.Vector.Unboxed
--
--   TODO.  Use vectors of fixed length
meanU :: (Eq a, Fractional a, U.Unbox a) => L' (U.Vector a) (U.Vector a)
meanU = L' done step begin
  where
    begin = Pair U.empty 0 -- Pair sum nb_items
    done (Pair x _) = x
    step (Pair _ 0) x = Pair x 1
    step (Pair m n) x =
      let n' = n + 1
          m' = U.zipWith (+) m $ U.map (/ n') $ U.zipWith (-) x m
      in Pair m' n'
{-# INLINABLE meanU #-}

variance :: Fractional a => L' a a
variance = L' done step begin
  where
    begin = Pair3 0 0 0

    step (Pair3 n mean_ m2) x = Pair3 n' mean' m2'
      where
        n'     = n + 1
        mean'  = (n * mean_ + x) / (n + 1)
        delta  = x - mean_
        m2'    = m2 + delta * delta * n / (n + 1)

    done (Pair3 n _ m2) = m2 / n
{-# INLINABLE variance #-}

std :: Floating a => L' a a
std = sqrt variance
{-# INLINABLE std #-}

data TtestState a = TS !Int !a !a !a !a

-- | Welch's t-test
--   with numerically stable versions of mean and standard deviation
--   from Knuth, II.A.2.2, eqn (15) and (16)
--
--   This implementation assumes that the two populations have the
--   same sizes, since each sample of each population is provided for
--   each 'iteration' of the fold.
ttest :: Floating a => L' (a, a) a
ttest = L' done step begin
  where
    begin = TS 0 0 0 0 0
    step (TS 0 _ _ _ _)     (x, y) = TS 1 x y 0 0
    step (TS n m0 m1 s0 s1) (x, y) = TS n' m0' m1' s0' s1'
      where
        n'  = n + 1
        m0' = m0 + (x - m0) / (fromIntegral n')
        m1' = m1 + (y - m1) / (fromIntegral n')
        s0' = s0 + ((x - m0) * (x - m0'))
        s1' = s1 + ((y - m1) * (y - m1'))
    done (TS n m0 m1 s0 s1) = (m0 - m1) / (sqrt ((s0 + s1) / (fromIntegral $ n*n)))
    -- where
    -- std0 = sqrt (s0 / n)
    -- s0' = s0 / (n*n)
    -- s1' = s1 / (n*n)
{-# INLINABLE ttest #-}

-- | Welch's t-test for Data.Vector.Unboxed
--
-- TODO compute the max t-value at the same time
ttestU :: (Floating a, U.Unbox a) => L' (U.Vector a, U.Vector a) (U.Vector a)
ttestU = L' done step begin
  where
    begin = TS 0 U.empty U.empty U.empty U.empty   -- n m0 m1 s0 s1
    step (TS 0 _ _ _ _)     (x, y) = TS 1 x y zeros zeros
      where
        zeros = U.map (const 0) x -- Assuming the two vectors have the same size
    step (TS n m0 m1 s0 s1) (x, y) = TS n' m0' m1' s0' s1'
      where
        n'  = n + 1
        m0' = U.zipWith (+) m0 $ U.map (/ (fromIntegral n')) $ U.zipWith (-) x m0
        m1' = U.zipWith (+) m1 $ U.map (/ (fromIntegral n')) $ U.zipWith (-) y m1
        s0' = U.zipWith (+) s0 $ U.zipWith (*) (U.zipWith (-) x m0) (U.zipWith (-) x m0')
        s1' = U.zipWith (+) s1 $ U.zipWith (*) (U.zipWith (-) y m1) (U.zipWith (-) y m1')
    done (TS n m0 m1 s0 s1) =
      U.zipWith (/) (U.zipWith (-) m0 m1)
                    (U.map sqrt (U.map (/ (fromIntegral $ n * n)) (U.zipWith (+) s0 s1)))
{-# INLINABLE ttestU #-}

data PearsonState a = PS !Int !a !a !a !a !a

{-| Pearson correlation, single-pass version.

  r = Cov(X,Y) / stdDev(X) / stdDev(Y)
  r = Cov(X,Y) / sqrt(Var(X)) / sqrt(Var((Y))

  [1] provides a version that is more stable than [2], and closer from
  the two-pass 'standard' implementation.

  [1] http://mathforum.org/kb/message.jspa?messageID=432819
  [2] https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online

Extract from [1]:

Here is the single-pass algorithm for updating means and sums of
squares and products of deviations from the mean. It is fairly
well-conditioned. You should be able to translate this into Fortran,
Pascal, Basic or whatever language you choose.

n = 0
xbar = 0, ybar = 0
sxx = 0, sxy = 0, syy = 0
repeat
read x, y
n = n + 1
devx = x - xbar, devy = y - ybar
xbar = xbar + devx/n, ybar = ybar + devy/n
sxx = sxx + devx*(x - xbar)
sxy = sxy + devx*(y - ybar)
syy = syy + devy*(y - ybar)
until end of data
-}
pearson :: Floating a => L' (a, a) a
pearson = L' done step begin
  where
    begin = PS 0 0 0 0 0 0         -- n xbar ybar sxx sxy syy
    step (PS n xbar ybar sxx sxy syy) (x, y) = PS n' xbar' ybar' sxx' sxy' syy'
      where
        n' = n + 1
        devx = x - xbar
        devy = y - ybar
        xbar' = xbar + devx / fromIntegral n'
        ybar' = ybar + devy / fromIntegral n'
        sxx' = sxx + devx*(x - xbar')
        sxy' = sxy + devx*(y - ybar')
        syy' = syy + devy*(y - ybar')
    done (PS _ _ _ sxx sxy syy) = sxy / sqrt (sxx * syy)
{-# INLINABLE pearson #-}


pearsonUU :: (Floating a, U.Unbox a) => L' (U.Vector a, U.Vector a) (U.Vector a)
pearsonUU = L' done step begin
  where
    begin = PS 0 U.empty U.empty U.empty U.empty U.empty
    step (PS 0 _ _ _ _ _) (x, y) = PS 1 x y sxx' sxy' syy'
      where
        zeros = U.map (const 0) x -- Assuming the two vectors have the same size
        sxx' = zeros -- sxx + devx*(x - xbar') = 0 + x * (x - x)
        sxy' = zeros
        syy' = zeros
    step (PS n xbar ybar sxx sxy syy) (x, y) = PS n' xbar' ybar' sxx' sxy' syy'
      where
        n' = n + 1
        devx = U.zipWith (-) x xbar
        devy = U.zipWith (-) y ybar
        xbar' = U.map (/ fromIntegral n') $ U.zipWith (+) xbar devx
        ybar' = U.map (/ fromIntegral n') $ U.zipWith (+) ybar devy
        sxx' = U.zipWith (+) sxx $ U.zipWith (*) devx $ U.zipWith (-) x xbar'
        sxy' = U.zipWith (+) sxy $ U.zipWith (*) devx $ U.zipWith (-) y ybar'
        syy' = U.zipWith (+) syy $ U.zipWith (*) devy $ U.zipWith (-) y ybar'
    done (PS _ _ _ sxx sxy syy) =
      U.zipWith (/) sxy $ U.map sqrt $ U.zipWith (*) sxx syy
{-# INLINABLE pearsonUU #-}

pearsonUUx :: (Floating a, Ord a, U.Unbox a) => L' (U.Vector a, U.Vector a) (U.Vector a, U.Vector a)
pearsonUUx = L' done step begin
  where
    begin = PSUx 0 U.empty U.empty U.empty U.empty U.empty U.empty
    step (PSUx 0 _ _ _ _ _ _) (x, y) = PSUx 1 x y sxx' sxy' syy' max'
      where
        zeros = U.map (const 0) x -- Assuming the two vectors have the same size
        sxx' = zeros -- sxx + devx*(x - xbar') = 0 + x * (x - x)
        sxy' = zeros
        syy' = zeros
        max' = U.fromList [1] -- all correlation values are supposed to be '1' at the beginning.
    step (PSUx n xbar ybar sxx sxy syy m) (x, y) = PSUx n' xbar' ybar' sxx' sxy' syy' max'
      where
        n' = n + 1
        devx = U.zipWith (-) x xbar
        devy = U.zipWith (-) y ybar
        xbar' = U.map (/ fromIntegral n') $ U.zipWith (+) xbar devx
        ybar' = U.map (/ fromIntegral n') $ U.zipWith (+) ybar devy
        sxx' = U.zipWith (+) sxx $ U.zipWith (*) devx $ U.zipWith (-) x xbar'
        sxy' = U.zipWith (+) sxy $ U.zipWith (*) devx $ U.zipWith (-) y ybar'
        syy' = U.zipWith (+) syy $ U.zipWith (*) devy $ U.zipWith (-) y ybar'
        c = U.zipWith (/) sxy $ U.map sqrt $ U.zipWith (*) sxx syy
        max' = m `U.snoc` U.maximum c
    done (PSUx _ _ _ sxx sxy syy m) =
      ( U.zipWith (/) sxy $ U.map sqrt $ U.zipWith (*) sxx syy
      , m
      )
{-# INLINABLE pearsonUUx #-}

{-| Pearson's correlation between a stream of vectors and a stream of scalars.

Returns a vector of correlation values, and the history of the maximum correlation values over the vector of correlation values.
-}
pearsonUx :: (Floating a, Ord a, U.Unbox a) => L' (U.Vector a, a) (U.Vector a, U.Vector a)
pearsonUx = L' done step begin
  where
    begin = PSUx 0 U.empty 0 U.empty U.empty 0 U.empty
    step (PSUx 0 _ _ _ _ _ _) (x, y) = PSUx 1 x y sxx' sxy' syy' max'
      where
        zeros = U.map (const 0) x -- Assuming the two vectors have the same size
        sxx' = zeros -- sxx + devx*(x - xbar') = 0 + x * (x - x)
        sxy' = zeros
        syy' = 0
        max' = U.fromList [1] -- all correlation values are supposed to be '1' at the beginning.
    step (PSUx n xbar ybar sxx sxy syy m) (x, y) = PSUx n' xbar' ybar' sxx' sxy' syy' max'
      where
        n' = n + 1
        devx = U.zipWith (-) x xbar
        devy = y - ybar
        xbar' = U.zipWith (+) xbar $ U.map (/ fromIntegral n') devx
        ybar' = ybar + devy / fromIntegral n'
        sxx' = U.zipWith (+) sxx $ U.zipWith (*) devx $ U.zipWith (-) x xbar'
        sxy' = U.zipWith (+) sxy $ U.map (* (y - ybar')) devx
        syy' = syy + devy * (y - ybar')
        c = U.zipWith (/) sxy $ U.map (\q -> sqrt (q * syy)) sxx
        max' = m `U.snoc` U.maximum c
    done (PSUx _ _ _ sxx sxy syy m) =
        ( U.zipWith (/) sxy $ U.map (\x -> sqrt (x * syy)) sxx
        , m
        )
{-# INLINABLE pearsonUx #-}

data PearsonStateUx a b =
  PSUx !Int  -- index of the current iteration
       !a    -- mean of x (vector)
       !b    -- mean of y (scalar)
       !a    -- variance of x
       !a    -- covariance(x,y)
       !b    -- variance of y
       !a    -- history of max values of Pearson's correlation coefficient

pearsonLL :: (Floating a) => L' ([a], [a]) ([a])
pearsonLL = L' done step begin
  where
    begin = PS 0 [] [] [] [] []
    step (PS 0 _ _ _ _ _) (x, y) = PS 1 x y sxx' sxy' syy'
      where
        zeros = map (const 0) x -- Assuming the two vectors have the same size
        sxx' = zeros -- sxx + devx*(x - xbar') = 0 + x * (x - x)
        sxy' = zeros
        syy' = zeros
    step (PS n xbar ybar sxx sxy syy) (x, y) = PS n' xbar' ybar' sxx' sxy' syy'
      where
        n' = n + 1
        devx = L.zipWith (-) x xbar
        devy = L.zipWith (-) y ybar
        xbar' = L.map (/ fromIntegral n') $ L.zipWith (+) xbar devx
        ybar' = L.map (/ fromIntegral n') $ L.zipWith (+) ybar devy
        sxx' = L.zipWith (+) sxx $ L.zipWith (*) devx $ L.zipWith (-) x xbar'
        sxy' = L.zipWith (+) sxy $ L.zipWith (*) devx $ L.zipWith (-) y ybar'
        syy' = L.zipWith (+) syy $ L.zipWith (*) devy $ L.zipWith (-) y ybar'
    done (PS _ _ _ sxx sxy syy) =
      L.zipWith (/) sxy $ L.map sqrt $ L.zipWith (*) sxx syy
{-# INLINABLE pearsonLL #-}

  -- TODO #0. implement this.
  -- TODO #1. integration dans scarlet
  -- TODO #2. ad hoc program for CPA analysis, using Chart

instance Num b => Num (L' a b) where
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

    negate = fmap negate
    {-# INLINE negate #-}

    abs = fmap abs
    {-# INLINE abs #-}

    signum = fmap signum
    {-# INLINE signum #-}

    (+) = liftA2 (+)
    {-# INLINE (+) #-}

    (*) = liftA2 (*)
    {-# INLINE (*) #-}

    (-) = liftA2 (-)
    {-# INLINE (-) #-}

instance Fractional b => Fractional (L' a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance Floating b => Floating (L' a b) where
    pi = pure pi
    {-# INLINE pi #-}

    exp = fmap exp
    {-# INLINE exp #-}

    sqrt = fmap sqrt
    {-# INLINE sqrt #-}

    log = fmap log
    {-# INLINE log #-}

    sin = fmap sin
    {-# INLINE sin #-}

    tan = fmap tan
    {-# INLINE tan #-}

    cos = fmap cos
    {-# INLINE cos #-}

    asin = fmap sin
    {-# INLINE asin #-}

    atan = fmap atan
    {-# INLINE atan #-}

    acos = fmap acos
    {-# INLINE acos #-}

    sinh = fmap sinh
    {-# INLINE sinh #-}

    tanh = fmap tanh
    {-# INLINE tanh #-}

    cosh = fmap cosh
    {-# INLINE cosh #-}

    asinh = fmap asinh
    {-# INLINE asinh #-}

    atanh = fmap atanh
    {-# INLINE atanh #-}

    acosh = fmap acosh
    {-# INLINE acosh #-}

    (**) = liftA2 (**)
    {-# INLINE (**) #-}

    logBase = liftA2 logBase
    {-# INLINE logBase #-}


-- | A strict version of @Maybe@.
data Maybe' a = Nothing' | Just' !a
toLazy :: Maybe' a -> Maybe a
toLazy Nothing' = Nothing
toLazy (Just' x) = Just x

test :: IO ()
test = do
  let l :: [Float]
      l = [1..100]

  -- run
  print $ run l sum
  -- prefix
  do
    let sum' = prefix l sum
    print $ run [] sum'
  print $ run l mean
  print $ run [(1::Float)..10000000] mean
  print $ flip run mean [(1::Double)..10000000]
  print $ flip run mean [(1::Float)..10000000]
  print $ run l std
  let average = (/) <$> sum <*> genericLength
  print $ run [(1::Double)..10000000] average
  print $ flip run average [(1::Double)..10000000]
