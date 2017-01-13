{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative

import           Aes
import           Aes.Hypothesis
import           AesImport
import           TTest

data Opts = Opts
  { output      :: !FilePath
  , size        :: !Size
  , seed        :: !Seed
  , progCommand :: !Command
  }
newtype Seed = Seed Int
newtype Size = Size Int

data Command
  = FirstSBOX
  | TTestFR { fixed  :: FilePath
            , random :: FilePath
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
    FirstSBOX   -> do
      putStrLn "** CPA.  compute a random sequence of n plaintexts **"
      let Size n = size opts
      ts <- take n <$> randomPlaintexts
      exportTexts (output opts) ts

    TTestFR ff fr -> do
      putStrLn "** t-test. specific fixed vs. random **"

      let fixedtext :: Plaintext
          -- fixed value proposed by Goodwill et al., 2011
          fixedtext = stringImport "0x90 0x18 0x60 0x95 0xef 0xbf 0x55 0x32 0x0d 0x4b 0x6b 0x5e 0xee 0xa3 0x39 0xda"
          Size n = size opts
          txtf = take n $ repeat $ fixedtext
      txtr <- take n <$> randomPlaintexts
      exportTexts ff txtf
      exportTexts fr txtr

    TTestRR kfile b p0 p1 -> do
      putStrLn "** t-test. specific random vs. random **"

      -- read the key file
      keys <- importTexts kfile
      key <- case keys of
        []   -> error $ "Error. " <> kfile <> " does not contain a valid AES key value."
        k:[] -> return k
        k:_  -> do
          putStrLn "Warning.  More than one key found in the key file.  Proceeding with the first key value."
          return k

      -- compute the two populations of plaintexts
      pops <- ttestRR firstSBOX key b

      let Size n = size opts
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
      <*> ( Size <$> option auto
            ( long "nb"
              <> short 'n'
              <> metavar "NUMBER"
              <> help "Size of the set of plaintexts generated"
              <> value 1024
              <> showDefault
            )
          )
      <*> ( Seed <$> option auto
            ( long "seed"
              <> short 'x'
              <> metavar "SEED_VALUE"
              <> help "Seed of the random number generator"
              <> value 0
              <> showDefault
            )
          )
      <*> hsubparser (firstSboxCommand <> tTestFR <> tTestRR)

    firstSboxCommand = command "firstsbox"
                       ( info firstSboxOptions
                         ( progDesc "Compute hypothesis values for the first SBOX.")
                       )
    firstSboxOptions = pure FirstSBOX

    tTestFR = command "ttest-fr"
              ( info
                ( TTestFR
                  <$> strOption
                  ( long "fixed"
                    <> short '0'
                    <> value "fixed.txt"
                    <> showDefault
                    <> help "name of the output file containing the fixed plaintext values"
                  )
                  <*> strOption
                  ( long "random"
                    <> short '1'
                    <> value "randoms.txt"
                    <> showDefault
                    <> help "name of the output file containing the random plaintext values"
                  )
                )
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
