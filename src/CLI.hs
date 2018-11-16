{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | main CLI options

module CLI where

import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import qualified Data.Version        as V (showVersion)
import           Options.Applicative
import           Paths_haskell_aes   (version)

default (T.Text)

data Command = View ViewOptions
             | CPA CPAOptions

optParser :: Parser Command
optParser = hsubparser
  ( command "view" ( info cmdViewParser
                     ( progDesc "View traces"))
    <>  command "cpa" ( info cmdCPAParser
                        ( progDesc "CPA analysis"))
  )

data TraceData = TracesDir FilePath
               | TraceRawFile FilePath
               deriving (Show, Eq)

parseTraces :: Parser TraceData
parseTraces =
  ( TracesDir <$> strOption
       ( metavar "TRACES_DIR"
         <> help "Location of the directory with traces files, in textual format."
         <> long "traces-dir"
         <> short 'd'
       )
     )
      <|> ( TraceRawFile <$> strOption
            ( metavar "TRACE_RAWFILE"
              <> help "Location of the trace files, in raw format."
              <> long "trace-rawfile"
              <> short 'f'
            )
          )

parseTmin :: Parser Int
parseTmin =
  option (fromInteger <$> auto)
  ( long "tmin"
    <> metavar "TMIN"
    <> help "Sample number for the start of the observation window [default: 0]."
    <> value 0
  )

parseTmax :: Parser (Maybe Int)
parseTmax =
  optional ( option (fromInteger <$> auto)
             ( long "tmax"
               <> metavar "TMAX"
               <> help "Sample number for the end of the observation window [default: full trace length]."
             )
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

-- * View options

data ViewOptions = ViewOptions
  { traces   :: !TraceData
  , tmin     :: !Int          -- ^ the number of the first sample used
  , mtmax    :: !(Maybe Int)  -- ^ the number of the latest sample used
  , nbTraces :: !Int          -- ^ number of traces used for the CPA analysis
  } deriving (Show)

cmdViewParser :: Parser Command
cmdViewParser =
  View <$>
  ( ViewOptions
    <$> parseTraces
    <*> parseTmin
    <*> parseTmax
    <*> parseNbTraces 16
  )

-- * CPA Options

data CPAOptions = CPAOptions
  { traces   :: !TraceData
  , tmin     :: !Int          -- ^ the number of the first sample used
  , mtmax    :: !(Maybe Int)  -- ^ the number of the latest sample used
  , textFile :: !FilePath
  , keyFile  :: !(Maybe FilePath)
  , nbTraces :: !Int          -- ^ number of traces used for the CPA analysis
  , byteOpt  :: !Int
  } deriving (Show)

cmdCPAParser :: Parser Command
cmdCPAParser = CPA <$>
  ( CPAOptions
    <$> parseTraces
    <*> parseTmin
    <*> parseTmax
    <*> strOption ( long "textfile" <> short 't'
                    <> metavar "TEXTFILE"
                    <> help "Location of the plaintexts file  [default: TRACES_DIR/plaintexts.txt]"
                  )
    <*> optional ( strOption
                   ( long "keyfile" <> short 'k'
                     <> metavar "KEYFILE"
                     <> help "Location of the key file"
                   )
                 )
    <*> parseNbTraces 512
    <*> option (fromInteger <$> auto)
    ( long "byte" <> short 'b'
      <> metavar "BYTE"
      <> help "Number of the key byte to attack [default: 0]"
      <> value 0
    )
  )

-- * option parsers
parseNbTraces :: Int -> Parser Int
parseNbTraces n = option (fromInteger <$> auto)
  ( long "nbtraces" <> short 'n'
    <> metavar "NSIWE"
    <> help "Number of traces used for the CPA analysis [default: 512]"
    <> value n
  )
