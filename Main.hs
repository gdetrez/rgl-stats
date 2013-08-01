module Main where

import HtmlReport
import Options
import Shelly
import Languages
import Status
import Data.Monoid ((<>))

main :: IO ()
main = do
  options <- getOptions
  setLogger options
  shelly $ silently $ do
    debug $ show options
    status <- mapM getStatus' languages
    case htmlReport options of
      Just p  -> writefile p (makeHtmlReport status)
      Nothing -> return ()
  where getStatus' l = do notice ("\x2192 " <> show (name l))
                          getStatus l
