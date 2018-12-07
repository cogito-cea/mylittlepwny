-----------------------------------------------------------------------------
-- |
-- Module      :  CLI
-- Copyright   :  (c) CEA 2018
-- License     :  CeCILL-B (see the file LICENSE)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- Main module for the command-line interface.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module CLI
  ( cli
  ) where

import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import qualified Data.Version        as V (showVersion)
import           Options.Applicative
import           Paths_mylittlepwny   (version)

import           CLI.Internal
import           CPA
import           TTest
import           View

default (T.Text)

cli :: IO ()
cli = do
  cmd <- parseCLI
  case cmd of
    View o    -> viewTraces o
    CPA o     -> cpa o
    TTestNS o -> ttestNonSpecific o
    TTestS  o -> ttestSpecific o

parseCLI :: IO Command
parseCLI = execParser optInfo

data Command = View ViewOptions
             | CPA CPAOptions
             | TTestNS TTestNonSpecificOptions
             | TTestS  TTestSpecificOptions

optParser :: Parser Command
optParser = hsubparser
  ( command "view" ( info (View <$> cmdViewParser)
                     ( progDesc "View traces"))
    <>  command "cpa" ( info (CPA <$> cmdCPAParser)
                        ( progDesc "CPA analysis"))
    <>  command "ttest" ( info (TTestNS <$> cmdTTestNSParser)
                        ( progDesc "Non-specific t-test"))
    <>  command "specific" ( info (TTestS <$> cmdTTestSParser)
                        ( progDesc "Specific t-test"))
  )

optInfo :: ParserInfo Command
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
