{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Status where

import Languages
import Shelly
import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Exception (Exception)
default (T.Text)

data Status = Status { lang :: Lang,
  lexiconStatus :: Either Text (Int,Int),
  syntaxStatus :: Either Text (Int,Int),
  irregStatus :: Either Text (Int,Int),
  dictStatus :: Either Text (Int,Int) }

getStatus :: Lang -> Sh Status
getStatus l = do
  lex    <- getModuleStatus (lexiconModule l)
  syntax <- getModuleStatus (syntaxModule l)
  irreg  <- getModuleStatus (irregModule l)
  dict   <- getModuleStatus (dictModule l)
  return (Status l lex syntax irreg dict)

getModuleStatus :: Maybe FilePath -> Sh (Either Text (Int,Int))
getModuleStatus Nothing = return (Left "Skipped")
getModuleStatus (Just p) = flip catchany_sh onErrors $ do
  exists <- test_f p
  if not exists
    then return $ Left (toTextIgnore p <> " does not exists")
    else do
      funs <- pg_funs p
      missing <- pg_missing p
      return (Right (missing, funs))
  where pg_missing p = do
          out <- setStdin "pg -missing" >> gf p
          return (length (drop 2 (T.words out)))
        pg_funs p = do
          out <- setStdin "pg -funs" >> gf p
          return (length (filter (not.T.null) (T.lines out)))
        gf p = cmd "gf" "--run" p "+RTS" "-K32M" "-RTS"
        onErrors :: (Exception e, Show e) => e -> Sh (Either Text (Int,Int))
        onErrors e = return (Left (T.pack (show e)))
