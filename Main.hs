{-# LANGUAGE OverloadedStrings #-}
module Main where

import HtmlReport
import Options
import Shelly
import Languages
import Status
import Predictability
import Data.Monoid ((<>))
import Prelude hiding (FilePath)

main :: IO ()
main = do
  options <- getOptions
  setLogger options
  shelly $ silently $ do
    debug $ show options
    -- The first thing we do is to make sure we can find the gf binary.
    -- Otherwise we exit with an error
    gf <- findGf (gfBin options)
    notice $ "Using gf binary: " ++ show gf

    status <- mapM (getStatus' options gf) languages
    case htmlReport options of
      Just p  -> writefile p (makeHtmlReport status)
      Nothing -> return ()
  where getStatus' options gf l = do
          notice ("\x2192 " <> show (name l))
          getStatus options gf l


-- | Function that tries to find the gf binaries given the --gf-bin option
findGf :: Maybe FilePath -> Sh FilePath
findGf (Nothing) = do
    gf <- which "gf"
    case gf of
      Just p -> return p
      Nothing -> errorExit "gf is not in your path. Please specify the path to the gf binary with --gf-bin"
findGf (Just p) = do
    p' <- canonic p
    exists <- test_e p'
    unless exists $ errorExit "The specified gf binary does not exists"
    return p'

