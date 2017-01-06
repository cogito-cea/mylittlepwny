{-# LANGUAGE RankNTypes #-}

module Aes.Util
  where

(°) :: forall b c a. (a -> b) -> (b -> c) -> a -> c
(°) = flip (.)

($$) :: forall c a. a -> (a -> c) -> c
($$) = flip ($)
