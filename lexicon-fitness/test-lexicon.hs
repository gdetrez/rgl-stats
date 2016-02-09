module Main where

import qualified Data.Set as Set
import Data.Set (Set,(\\))
import Control.Monad (liftM)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Options.Applicative
import Text.CSV
import Data.String.Utils (strip)
import System.Log.Logger
import Control.Monad.Writer
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import System.FilePath (replaceExtension)

data Parameters = P
  { pWordlist :: FilePath
  , pLexicon  :: FilePath }
  deriving (Show)

parameters :: Parser Parameters
parameters = P
    <$> strOption
        ( long "wordlist"
        <> metavar "WORDLIST"
        <> help "Wordlist file" )
    <*> strArgument
        ( metavar "LEXICON"
        <> help "CSV file, one entry per line" )

main = do
    updateGlobalLogger "" (setLevel INFO)
    params <- execParser opts
    wordlist <- readWordList (pWordlist params)
    lexicon <- readLexicon (pLexicon params)

    -- Compute coverage
    let missing = wordlist \\ Set.fromList (concat lexicon)
    let coverage = percentage (Set.size wordlist - Set.size missing) (Set.size wordlist)
    printf "Coverage: %.2f%%\n" coverage
    let missingFile = replaceExtension (pLexicon params) "MISSING"
    writeFile missingFile (unlines (Set.toList missing))
    infoM "" $ "Error forms saved in " ++ missingFile


    -- Compute correctness
    let (fitness, errors) = computeCorrectness lexicon wordlist

    printf "Fitness: %.2f%%\n" (mean fitness)
    printf "\n"
    printTable ("Fitness","Occurences")
               (reverse (MS.toAscOccurList (MS.fromList fitness)))
    let errorFile = replaceExtension (pLexicon params) "ERRORS"
    writeFile errorFile (unlines errors)
    infoM "" $ "Error forms saved in " ++ errorFile


  where opts = info (helper <*> parameters) (fullDesc)

printTable :: (String,String) -> [(Double,Int)] -> IO ()
printTable (ha,hb) d = do
  printf "\t%s\t%s\n" ha hb
  mapM_ (\(a,b) -> printf "\t%.2f\t%d\n" a b) d

-- Compute the "correctness" of a lexicon given a wordlist.
-- In this context, correctness is defined as the inverse of the coverage:
-- each form in the lexicon which is not present in the wordlist is considered
-- incorrect. (This is what a basic spellchecker does.)
computeCorrectness :: [[String]] -> Set String -> ([Double], [String])
computeCorrectness lexicon wordlist = runWriter $ do
    mapM entryCorrectness lexicon
  where entryCorrectness e = do
          correct <- liftM (length . filter id) $ mapM isCorrect e
          let total = length e
          return (percentage correct total)
        isCorrect form = if Set.member form wordlist
            then return True
            else tell [form] >> return False

readWordList :: FilePath -> IO (Set String)
readWordList path =do
  wl <- readFile path >>= return . lines
  infoM "Wordlist" (printf "%d word forms read from %s" (length wl) path)
  return (Set.fromList wl)

readLexicon :: FilePath -> IO [[String]]
readLexicon path = do
  csvdata <- parseCSVFromFile path
  case csvdata of
      Left err -> errorM "Lexicon" (show err) >> exitFailure
      Right l -> do
          infoM "Lexicon" (printf "%d full form entries read from %s" (length l) path)
          return (map (map strip) l)

-- Compute the ratio between two integer in percents
percentage :: Int -> Int -> Double
percentage a b = 100 * (fromIntegral a / fromIntegral b)

mean :: [Double] -> Double
mean l = let total = fromIntegral (length l) in sum l / total
