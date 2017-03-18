{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Version        (showVersion)
import           Options.Applicative
import qualified Paths_haskell_aes   as V (version)
import           System.Exit         (exitSuccess)
import System.Random (randoms, getStdGen)

import           Aes
import           Aes.Hypothesis
import           AesImport

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
               }
  | FirstSBOX { plaintexts :: !FilePath
              , byte       :: !Int
              }
  | TTestFR { textFile :: !FilePath
            , sprtFile :: !FilePath
            }
  | TTestRR { keyFile  :: !FilePath
            , bitnb    :: !BitNumber
            , textFile :: !FilePath
            , sprtFile :: !FilePath
            , ciphFile :: Maybe FilePath
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

    TTestFR pfile sepfile -> do
      putStrLn "** non-specific t-test. fixed vs. random **"
      let fixedtext :: Plaintext
          -- fixed value proposed by Goodwill et al., 2011
          fixedtext = stringImport "0x90 0x18 0x60 0x95 0xef 0xbf 0x55 0x32 0x0d 0x4b 0x6b 0x5e 0xee 0xa3 0x39 0xda"

      -- a list of random Bool values
      bools <- randoms <$> getStdGen

      -- a list of random Plaintexts
      texts <- randomPlaintexts

      let sepfun :: [Plaintext] -> [Bool] -> [(Plaintext, Int)]
          sepfun ps bs = zipWith go ps bs
            where
              go _ False = (fixedtext, 0)
              go p True  = (p,         1)

      let Size n = size opts

      let res = take n $ sepfun texts bools

      exportTexts pfile $ map fst res
      writeFile sepfile $ unlines $ map (show . snd) res

    TTestRR kfile b textfile sprtfile cfile -> do
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
      texts <- take n <$> randomPlaintexts

      -- compute the value of bit b at the ouput of the first SBOX
      -- i.e. 'sep' is a vector of Int values in {0, 1}
      let sep = map (bit b . firstSBOX key) texts

      -- store results
      exportTexts textfile texts
      writeFile sprtfile $ unlines $ map show sep
      case cfile of
        Nothing -> return ()
        Just f -> do
          exportTexts f $ map (aesBlockEncrypt key) texts

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
              <> value 16384
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
                       $ progDesc "Compute hypothesis values for the first AddRoundKey, using a Hamming weight model."

    firstSboxCommand = command "sbox"
                       $ info firstSboxOptions
                       $ progDesc "Compute hypothesis values for the first SBOX, using a Hamming weight model."

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
              $ info (TTestFR <$> txtOpt <*> sepOpt)
              $ progDesc $ unlines
              [ "Compute two populations of plaintexts for the non-specific t-test (fixed vs. random), for the output of the first SBOX."
              , "Generates two plaintext files named after the contents of options --population0 and --population1."
              ]

    tTestRR = command "ttest-rr"
              $ info (TTestRR <$> keyOpt <*> bitOpt <*> txtOpt <*> sepOpt <*> cphOpt)
              $ progDesc $ unlines
              [ "Compute two populations of plaintexts for the specific t-test (random vs. random), for the output of the first SBOX. "
              , "Generates a list of plaintexts and a list of values {0,1} to separate the two t-test populations. "
              ]

    keyOpt =
      strOption (
        long "key"
        <> short 'k'
        <> metavar "KEYFILE"
        <> help "the input key file"
      )
    bitOpt =
      ( bitNumber <$> option auto
        ( long "bit-number"
          <> short 'b'
          <> metavar "BIT_NUMBER"
          <> help "number of the state bit observed"
          <> value 0
          <> showDefault
        )
      )
    txtOpt =
      strOption
      ( long "plaintexts"
        <> short 'p'
        <> value "plaintexts.txt"
        <> showDefault
        <> help "Name of the output file containing the plaintext values for the two populations."
      )
    sepOpt =
      strOption
      ( long "separate"
        <> short 's'
        <> value "text-separate.txt"
        <> showDefault
        <> help "This generated files has the same length than the plaintext file.  It contains a list of integer values either 0 or 1, in order to separate the plaintexts between two populations '0' and '1'."
      )
    cphOpt =
      optional
      ( strOption $
        long "ciphers"
        <> short 'c'
        <> metavar "CIPHERS"
        <> help "Generate the lists of expected cipher values in file CIPHERS.  The file is not generated if this option is not used."
     )
