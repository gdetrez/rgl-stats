{-# LANGUAGE OverloadedStrings #-}
module Main where

import HtmlReport
import Options
import Shelly
import Languages
import Status
import Data.Monoid ((<>))
import Prelude hiding (FilePath)

main :: IO ()
main = shelly $ silently $ do
    options <- getOptions
    status <- mapM (getStatus' options) languages
    case htmlReport options of
      Just p  -> writefile p (makeStatusReport status)
      Nothing -> return ()
  where getStatus' options l = do
          notice ("\x2192 " <> show (name l))
          s <- getStatus options l
          liftIO $ print s
          return s
