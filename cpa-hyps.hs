{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           System.FilePath     (dropExtension, takeExtension)

import           Aes.Hypothesis
import           AesImport
import           TTest

data Opts = Opts
  { output      :: !FilePath
  , size        :: !Int
  , seed        :: !Int
  , progCommand :: !Command
  }

data Command
  = FirstSBOX
  | TTestFR { keyFile :: FilePath
            , bitnb   :: BitNumber
            }
  | TTestRR { keyFile :: FilePath
            , bitnb   :: BitNumber
            , pop0    :: FilePath
            , pop1    :: FilePath
            }

main :: IO ()
main = do
  -- parse program commands and options
  opts <- execParser optsParser
  -- the real program entry point
  case progCommand opts of
    FirstSBOX   -> putStrLn "FirstSBOX. TODO"

    TTestFR kfile b -> do
      putStrLn "TTestFR. TODO"

    TTestRR kfile b -> do
      putStrLn "** t-test. specific random vs. random **"

    TTestRR kfile b p0 p1 -> do
      putStrLn "** t-test. specific random vs. random **"

      -- read the key file
      -- TODO quid si le fichier est vide ?
      -- TODO quid si le fichier comporte plus d'une clé ?
      key <- head <$> importTexts kfile

      pops <- ttestRR firstSBOX key b
      let n = size opts
      print n
      exportTexts p0 (take n $ fst pops)
      exportTexts p1 (take n $ snd pops)

  putStrLn "** End of processing **"

  where
    optsParser :: ParserInfo Opts
    optsParser =
      info ( helper <*> programOptions )
           ( fullDesc
             <> header "cpa-hyps: compute hypothesis values for CPA attacks on AES."
             <> ( progDesc $ unlines
                  [ "Compute hypothesis values at the output of the first SBOX."
                  , "Outputs the results in a text file.  "
                  , ""
                  , "Use COMMAND --help to see the list of options supported by each command."
                  ]
                )
           )
    programOptions = Opts
      <$> strOption
          ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Name of the output file"
            <> value "output.txt"
            <> showDefault
          )
      <*> option auto
          ( long "nb"
            <> short 'n'
            <> metavar "NUMBER"
            <> help "Size of the set of plaintexts generated"
            <> value 1024
            <> showDefault
          )
      <*> option auto
          ( long "seed"
            <> short 'x'
            <> metavar "SEED_VALUE"
            <> help "Seed of the random number generator"
            <> value 0
            <> showDefault
          )
      <*> hsubparser (firstSboxCommand <> tTestFR <> tTestRR)

    firstSboxCommand = command "firstsbox"
                       ( info firstSboxOptions
                         ( progDesc "Compute hypothesis values for the first SBOX.")
                       )
    firstSboxOptions = pure FirstSBOX

    tTestFR = command "ttest-fr"
              ( info ( ttestOptions TTestFR )
                ( progDesc $ unlines
                  [ "Compute two populations of plaintexts for the specific fixed vs. random t-test, for the output of the first SBOX."
                  , "Generates two output files: FILE0.txt and FILE1.txt named after the contents of the --output option."
                  ]
                )
              )

    tTestRR = command "ttest-rr"
              ( info
                ( TTestRR
                  <$> ttestKey
                  <*> ttestBit
                  <*> strOption
                  ( long "population0"
                    <> short '0'
                    <> value "population0.txt"
                    <> showDefault
                    <> help "name of the output file containing the plaintext values for population 0"
                  )
                  <*> strOption
                  ( long "population1"
                    <> short '1'
                    <> value "population0.txt"
                    <> showDefault
                    <> help "name of the output file containing the plaintext values for population 1"
                  )
                )
                ( progDesc $ unlines
                  [ "Compute two populations of plaintexts for the specific random vs. random t-test, for the output of the first SBOX."
                  , "Generates two output files: FILE0.txt and FILE1.txt named after the contents of the --output option."
                  ]
                )
              )

    ttestKey = strOption
               ( long "key"
                 <> short 'k'
                 <> metavar "KEYFILE"
                 <> help "the input key file"
               )
    ttestBit = option auto
               ( long "bit-number"
                 <> short 'b'
                 <> metavar "BIT_NUMBER"
                 <> help "number of the state bit observed"
                 <> value 0
                 <> showDefault
               )

compute100000CPAHypothesis :: IO ()
compute100000CPAHypothesis = do
  texts <- importTexts "plaintexts.txt"
  let hyps = hammingWeight $ firstRoundSBOX 0 texts
  exportHypothesis "test.txt" hyps
