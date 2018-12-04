{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Traces.PicoScope
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple interface for loading side-channel traces from a PicoScope.
-- This module also establishes communication with an STM32 board for
-- running the target code during the acquisition of traces.
--
-- TODO complete module description. relies on an external program, i.e. pico-controller.
--
-----------------------------------------------------------------------------

module Traces.PicoScope
  ( Handle
  , HasTraces(..)
  , InitPico(..)
  , Trace
  , loadStm32Bin
  ) where

import           Control.Monad              (unless)
import           Data.IORef                 (IORef, modifyIORef', newIORef,
                                             readIORef)
import qualified Data.Vector.Unboxed        as U
import           Turtle                     (printf, w, (%))
import qualified Turtle                     as Tt

import           Aes
import           Traces.Internal
import qualified Traces.PicoScope.PicoScope as Pico
import qualified Traces.PicoScope.Stm32     as Stm32

instance HasTraces Handle where
  type InitData Handle = InitPico
  init = Traces.PicoScope.init
  close = Traces.PicoScope.close
  load = Traces.PicoScope.load
  loadWindow = Traces.PicoScope.loadWindow
  size = sizeB

data InitPico = InitPico
  { binFile :: FilePath -- ^ the binary file to load on the STM32
  , keyInit :: Aes.Key
  , txtInit ::  [Plaintext]
  } deriving (Show)

data Handle = Handle
  { stm32    :: Stm32.Handle
  , pico     :: Pico.Handle
  , key      :: Aes.Key
  , texts    :: IORef [Plaintext]
  , sizeB    :: !Int  -- ^ trace size, in bytes
  , sampleNb :: !Int  -- ^ trace size, in elements
  }


-- | Init everything: load a binary file on the STM32, and start the
--   communication with the PicoScope.
init :: InitPico -> IO Handle
init (InitPico binFile k ts) = do
  -- STM32 init
  let cfg = Stm32.Config
            (Tt.fromString binFile)
            (Just "./utils/st-flash")
            Nothing
  Right stm <- Stm32.init cfg

  -- pico init
  let cfg' = Pico.Config "./fifofile" 1000000000 10000
  pico <- Pico.init cfg'

  iot <- newIORef ts

  return $ Handle stm pico k iot undefined undefined

-- | Ask the STM32 for the encryption of a new plaintext message, and
--   check the resulting cipher text.  Return the EM trace measured
--   during encryption.
--
-- TODO tell the picoscope about tmin and tmax?
load :: (Num a, U.Unbox a) => Handle -> IO (Trace a)
load Handle{..} = do
  Pico.expectOn pico Pico.ready  -- wait for the scope to get ready

  -- send the plaintext
  txt <- head <$> readIORef texts
  Stm32.sendText stm32 txt
  modifyIORef' texts tail

  trace <- Pico.read pico  -- read the EM trace
  c <- Stm32.readCipher stm32    -- read the encrypted text

  -- check the value  of the encrypted text
  let expectedRes = aesBlockEncrypt key txt
  unless (c == expectedRes) $
    do putStrLn "[STM32] ... ERROR!"
       printf ("[STM32] ... got      : "%w%"\n") c
       printf ("[STM32] ... expected : "%w%"\n") expectedRes
  -- done
  return $ U.map fromIntegral . U.fromList $ trace

-- | load a trace from the PicoScope, and return only the values in
--   the time window of interest.  'load' is IO so we fetch /all/
--   trace samples before filtering out the samples that we don't
--   need.  Generally speaking, it would be better to filter to
--   samples of interest in the IO action so that we process less
--   elements.  However, in this specific case, we need to read *all*
--   the elements returned by the PicoScope to flush the communication
--   pipe.
--
-- MAYBE.  use picoscope options (TRIGGER and SIZE) to load only the
--   samples in our window of interest.
loadWindow :: (Num a, U.Unbox a) => Handle -> TMin -> TMax -> IO (Trace a)
loadWindow h (TMin tmin) (TMax tmax) =
  (U.take (tmax - tmin) . U.drop (tmin - 1)) <$> Traces.PicoScope.load h

-- | Close everything: close the connexion with the PicoScope, and the
--   connexion with the STM32.
close :: Handle -> IO ()
close Handle{..} = do
  Tt.echo "[scarlet]  closing everything"
  Stm32.close stm32
  Pico.close pico
  Tt.echo "[scarlet]                     ... done"

-- | Load a new bin file on the STM32
loadStm32Bin :: Handle -> FilePath -> IO ()
loadStm32Bin Handle{..} = Stm32.load stm32
