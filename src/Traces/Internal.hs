-----------------------------------------------------------------------------
-- |
-- Module      :  Traces.Internal
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------


module Traces.Internal where

import qualified Data.Vector.Unboxed as U

type Trace a = U.Vector a

class HasTraces handle where
  init :: FilePath -> IO handle
  close :: handle -> IO ()
  load :: (Read a, Num a, U.Unbox a) => handle -> IO (Trace a)
  load' :: (Read a, Num a, U.Unbox a) => handle -> Int -> Int -> IO (Trace a)
  size :: handle -> Int
