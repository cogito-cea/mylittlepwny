module CLI.Types where


data TraceData = TracesDir FilePath
               | TraceRawFile FilePath
               deriving (Show, Eq)
