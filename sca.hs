import           CLI
import           CPA
import           TTest
import           View


main :: IO ()
main = do
  cmd <- parseCLI

  case cmd of
    View o  -> viewTraces o
    CPA o   -> cpa o
    TTest o -> ttest o
