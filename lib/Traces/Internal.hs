{-# LANGUAGE TypeFamilies #-}

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

newtype TMin = TMin Int
  deriving (Eq, Show, Ord)
newtype TMax = TMax Int
  deriving (Eq, Show, Ord)

class HasTraces handle where
  type InitData handle
  init :: InitData handle -> IO handle
  close :: handle -> IO ()
  load :: (Read a, Num a, U.Unbox a) => handle -> IO (Trace a)
  loadWindow :: (Read a, Num a, U.Unbox a) => handle -> TMin -> TMax -> IO (Trace a)
  size :: handle -> Int
