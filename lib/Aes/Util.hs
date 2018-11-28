{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Aes.Util
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------


module Aes.Util
  where

(°) :: forall b c a. (a -> b) -> (b -> c) -> a -> c
(°) = flip (.)

($$) :: forall c a. a -> (a -> c) -> c
($$) = flip ($)
