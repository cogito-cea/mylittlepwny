{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Version        (showVersion)
import           Options.Applicative
import qualified Paths_haskell_aes   as V (version)
import           System.Exit         (exitSuccess)
import           System.FilePath

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
-- TODO tenir compte de la graine
newtype Seed = Seed Int
newtype Size = Size Int

data Command
  = Version
  | RandomPlaintexts
  | FirstAddRK { plaintexts :: !FilePath
               , byte       :: !Int
               -- TODO - model: HW | HD
               }
  | FirstSBOX { plaintexts :: !FilePath
              , byte       :: !Int
              -- TODO - model: HW | HD
              }
  | TTestFR { fixed  :: !FilePath
            , random :: !FilePath
            }
  | TTestRR { keyFile   :: !FilePath
            , bitnb     :: !BitNumber
            , popFiles  :: !FilePath
            , ciphFiles :: Maybe FilePath
            }

main :: IO ()
main = do
  -- parse program commands and options
  opts <- execParser optsParser

  -- the real program entry point
  case progCommand opts of

    Version -> putStrLn ("version " ++ showVersion V.version) >> exitSuccess

    RandomPlaintexts -> do
      putStrLn "** Generate random plaintexts **"
      let Size n = size opts
      ts <- take n <$> randomPlaintexts
      exportTexts (output opts) ts

    FirstAddRK f b -> do
      putStrLn "** CPA. compute the hypothetical values after the first AddRoundKey computation **"
      let Size n = size opts
      texts <- take n <$> importTexts f
      exportHypothesis (output opts) $ hammingWeight $ firstAddRK b texts

    FirstSBOX f b -> do
      putStrLn "** CPA. compute the hypothetical values after the first SBOX computation **"
      let Size n = size opts
      texts <- take n <$> importTexts f
      exportHypothesis (output opts) $ hammingWeight $ firstRoundSBOX b texts

    TTestFR ff fr -> do
      putStrLn "** t-test. specific fixed vs. random **"

      let fixedtext :: Plaintext
          -- fixed value proposed by Goodwill et al., 2011
          fixedtext = stringImport "0x90 0x18 0x60 0x95 0xef 0xbf 0x55 0x32 0x0d 0x4b 0x6b 0x5e 0xee 0xa3 0x39 0xda"
          Size n = size opts
          txtf = replicate n fixedtext
      txtr <- take n <$> randomPlaintexts
      exportTexts ff txtf
      exportTexts fr txtr

    TTestRR kfile b pbasename cbasename -> do
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
      let Size n = size opts
      pops <- ttestRR firstSBOX key b
      let pop0 = take n $ fst pops
          pop1 = take n $ snd pops

      -- store plaintext results
      let p0 = dropExtensions pbasename ++ "0" <.> "txt"
          p1 = dropExtensions pbasename ++ "1" <.> "txt"
      exportTexts p0 pop0
      exportTexts p1 pop1

      -- do we also store expected cipher values?
      case cbasename of
        Nothing -> return ()
        Just f -> do
          let c0 = dropExtensions f ++ "0" <.> "txt"
              c1 = dropExtensions f ++ "1" <.> "txt"
          exportTexts c0 $ map (aesBlockEncrypt key) pop0
          exportTexts c1 $ map (aesBlockEncrypt key) pop1

  putStrLn "** End of processing **"

  where
    optsParser :: ParserInfo Opts
    optsParser =
      info ( helper <*> programOptions )
           ( fullDesc
             <> header "cpa-hyps: compute hypothesis values for CPA attacks on AES."
             <> progDesc
             ( unlines
               [ "cpa-hyps: a few bunch of things to perform side channel attacks."
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
      <*> hsubparser ( version
                       <> randomPT
                       <> firstAddRKCommand
                       <> firstSboxCommand
                       <> tTestFR
                       <> tTestRR
                     )
    version  = command "version"
               $ info versionOptions
               $ progDesc "Print program version"

    versionOptions = pure Version

    randomPT = command "plaintexts"
               $ info randomPTOptions
               $ progDesc "Generate a list of random plaintexts"

    randomPTOptions = pure RandomPlaintexts

    firstAddRKCommand = command "addrk"
                       $ info firstAddRKOptions
                       $ progDesc "Compute hypothesis values for the first AddRoundKey."

    firstSboxCommand = command "sbox"
                       $ info firstSboxOptions
                       $ progDesc "Compute hypothesis values for the first SBOX."

    firstAddRKOptions = FirstAddRK
                       <$> strOption
                       ( long "plaintexts"
                         <> short 'p'
                         <> help "Name of the input file containing the plaintext values"
                       )
                       <*> option auto
                       ( long "byte"
                         <> short 'b'
                         <> help "Byte number in [0..15] used to compute CPA correlation hypothesis"
                         <> value 0
                         <> showDefault
                       )

    firstSboxOptions = FirstSBOX
                       <$> strOption
                       ( long "plaintexts"
                         <> short 'p'
                         <> help "Name of the input file containing the plaintext values"
                       )
                       <*> option auto
                       ( long "byte"
                         <> short 'b'
                         <> help "Byte number in [0..15] used to compute CPA correlation hypothesis"
                         <> value 0
                         <> showDefault
                       )

    tTestFR = command "ttest-fr"
              $ info tTestFROptions
              $ progDesc $ unlines
              [ "Compute two populations of plaintexts for the specific fixed vs. random t-test, for the output of the first SBOX."
              , "Generates two plaintext files named after the contents of options --population0 and --population1."
              ]

    tTestFROptions = TTestFR
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


    tTestRR = command "ttest-rr"
              $ info ttRROpts
              $ progDesc $ unlines
              [ "Compute two populations of plaintexts for the specific random vs. random t-test, for the output of the first SBOX."
              , "Generates two plaintext files named after the contents of options --population0 and --population1."
              ]

    ttRROpts = TTestRR <$>
      strOption (
        long "key"
        <> short 'k'
        <> metavar "KEYFILE"
        <> help "the input key file"
      )
      <*>
      ( bitNumber <$> option auto
        ( long "bit-number"
          <> short 'b'
          <> metavar "BIT_NUMBER"
          <> help "number of the state bit observed"
          <> value 0
          <> showDefault
        )
      )
      <*>
      strOption
      ( long "population0"
        <> short 'p'
        <> value "population"
        <> showDefault
        <> help (
          unlines [ "Basename of the output files containing the plaintext values for population 0. "
                  , "For example, with option --population pop, the program generates two files: "
                  , "  pop0.txt and pop1.txt"
                  ]
          )
      )
      <*>
      optional
      ( strOption $
        long "ciphers"
        <> short 'c'
        <> metavar "CIPHERS"
        <> help (
          unlines ["Generate the lists of expected cipher values in two files named after CIPHERS."
                  , "CIPHERS is the basename of the two files generated."
                  , "For example, with option --ciphers ciph, the program generates two files:"
                  , "  ciph0.txt which contains the expected ciphers for population 0,"
                  , "  and ciph1.txt which contains the expected ciphers for population 1."
                  ]
          )
      )
