{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Status where

import Control.Exception (Exception)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Prelude hiding (FilePath)
import Shelly

import Data.Text (Text)
import qualified Data.Text as T

import Predictability (ExperimentReport, runExperiment)
import Languages
import Options

default (T.Text)

data Status = Status { lang :: Lang,
  lexiconStatus :: Either Text (Int,Int),
  syntaxStatus :: Either Text (Int,Int),
  irregStatus :: Either Text (Int,Int),
  dictStatus :: Either Text (Int,Int) }

instance Show Status where
  show s = printf "Lexicon:%s Syntax:%s Irreg:%s Dict:%s"
            (pp (lexiconStatus s)) (pp (syntaxStatus s))
            (pp (irregStatus s)) (pp (dictStatus s))
    where pp (Left _) = "ERROR" :: String
          pp (Right (a,b)) = printf "%d/%d" (b-a) b :: String

getStatus :: Options -> Lang -> Sh Status
getStatus opts l = do
  lex    <- getModuleStatus gf (lexiconModule l)
  syntax <- getModuleStatus gf (syntaxModule l)
  irreg  <- getModuleStatus gf (irregModule l)
  dict   <- getModuleStatus gf (dictModule l)
  return (Status l lex syntax irreg dict)
  where gf = fromMaybe "gf" (gfBin opts)

getModuleStatus :: FilePath -> Maybe FilePath -> Sh (Either Text (Int,Int))
getModuleStatus _ Nothing = return (Left "Skipped")
getModuleStatus gf (Just p) = flip catchany_sh onErrors $ do
  exists <- test_f p
  if not exists
    then return $ Left (toTextIgnore p <> " does not exists")
    else do
      funs <- pg_funs p
      missing <- pg_missing p
      return (Right (missing, funs))
  where pg_missing p = do
          out <- setStdin "pg -missing" >> rungf p
          return (length (drop 2 (T.words out)))
        pg_funs p = do
          out <- setStdin "pg -funs" >> rungf p
          return (length (filter (not.T.null) (T.lines out)))
        rungf p = cmd gf "--run" p "+RTS" "-K32M" "-RTS"
        onErrors :: (Exception e, Show e) => e -> Sh (Either Text (Int,Int))
        onErrors e = return (Left (T.pack (show e)))
