module CLI.Internal
  ( parseNbTraces
  , parseTraces
  , parseTmin
  , parseTmax
  , parseTextFile
  , parseKeyFile
  , TraceData(..)
  ) where

import           Options.Applicative

data TraceData = TracesDir FilePath
               | TraceRawFile FilePath
               deriving (Show, Eq)

-- * option parsers

parseNbTraces :: Int -> Parser Int
parseNbTraces n = option (fromInteger <$> auto)
  ( long "nbtraces" <> short 'n'
    <> metavar "NSIWE"
    <> help "Number of traces used for the CPA analysis [default: 512]"
    <> value n
  )

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

parseTextFile :: Parser FilePath
parseTextFile =
  strOption ( long "textfile"
              <> short 't'
              <> metavar "TEXTFILE"
              <> help "Location of the plaintexts file"
            )

parseKeyFile :: Parser FilePath
parseKeyFile =
  strOption ( long "keyfile"
              <> short 'k'
              <> metavar "KEYFILE"
              <> help "Location of the key file"
            )
