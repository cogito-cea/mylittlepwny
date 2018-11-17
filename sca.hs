import           Options.Applicative

import           CLI
import           CPA
import           TTest
import           View


-- TODO t-test
-- TODO nouvelle méthode Pearson -> scarlet
-- TODO t-test -> scarlet
-- TODO optimiser le chargement des traces: format binaire, un seul fichier ou plusieurs ? (pour l'instant, c'est au moins la moitié du temps de calcul !!)

main :: IO ()
main = do
  cmd <- execParser optInfo

  case cmd of
    View o  -> viewTraces o
    CPA o   -> cpa o
    TTest o -> ttest o
