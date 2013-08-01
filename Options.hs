{-# LANGUAGE OverloadedStrings #-}
module Options where

import Options.Applicative
import Shelly (FilePath(..),Sh(..),liftIO)
import Prelude hiding (FilePath)
import Data.String (fromString)
import System.Log.Logger

data Options = Options
  { gfBin       :: Maybe FilePath
  , priority    :: Priority
  , htmlReport  :: Maybe FilePath
  , csvReport   :: Maybe FilePath
  , limit       :: Maybe Int
  } deriving (Eq,Show)

defaultOptions = Options  { gfBin = Nothing, priority = DEBUG
                          , htmlReport = Nothing, csvReport = Nothing
                          , limit = Nothing }

parseVerbosity :: Parser Priority
parseVerbosity =
            flag' INFO ( long "verbose" <> help "Enable verbose mode")
        <|> flag' DEBUG ( long "debug" <> help "Enable debug mode")
        <|> flag' WARNING ( long "quiet" <> help "Print less information")
        <|> pure NOTICE

parseOptions :: Parser Options
parseOptions = Options
      <$> optional (nullOption
          ( long "gf-bin"
         <> metavar "GF"
         <> help "Specify the gf binary to use instead of the one from $PATH"
         <> reader (Right . fromString)))
      <*> parseVerbosity
      <*> optional (nullOption
          ( long "html-report"
         <> metavar "HTML_FILE"
         <> help "Save a html formated report in HTML_FILE"
         <> reader (Right . fromString)))
      <*> optional (nullOption
          ( long "csv-report"
         <> metavar "CSV_FILE"
         <> help "Save mean costs in a csv file to be ploted by jenkins"
         <> reader (Right . fromString)))
      <*> optional (option
          ( long "limit"
         <> metavar "N"
         <> help "limit the experiment to N lexicon entries"))

getOptions :: IO Options
getOptions = execParser opts
  where opts = Options.Applicative.info (helper <*> parseOptions)
          ( fullDesc
         <> progDesc "Print a greeting for TARGET"
         <> header "hello - a test for optparse-applicative" )

loggerName :: String
loggerName = ""

setLogger :: Options -> IO ()
setLogger options = updateGlobalLogger loggerName (setLevel (priority options))

debug :: String -> Sh ()
debug = liftIO . debugM loggerName

info :: String -> Sh ()
info = liftIO . infoM loggerName

notice :: String -> Sh ()
notice = liftIO . noticeM loggerName

