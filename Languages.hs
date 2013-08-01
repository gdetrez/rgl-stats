{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Languages where

import Shelly (FilePath,(<.>),(</>))
import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
default (T.Text)

data Lang = Lang { name :: Text, iso :: Text,
    lexiconModule :: FilePath, syntaxModule :: FilePath,
    irregModule :: FilePath, dictModule :: FilePath }
  deriving (Eq,Show)

mkLang :: Text -> Lang
mkLang n = Lang n iso' (mkModule "Lexicon" iso') (mkModule "Lang" iso')
                       (mkModule "Irreg" iso') (mkModule "Dict" iso')
  where iso' = T.take 3 n

setIso :: Lang -> Text -> Lang
setIso l newIso = l { iso = newIso,
    lexiconModule = (mkModule "Lexicon" newIso),
    syntaxModule  = (mkModule "Lang" newIso),
    irregModule   = (mkModule "Irreg" newIso),
    dictModule    = (mkModule "Dict" newIso) }
  where oldIso = iso l

mkModule :: Text -> Text -> FilePath
mkModule prefix code =
  "dist"</>"build"</>"rgl"</>"alltenses"</>(prefix <> code)<.>"gfo"

languages :: [Lang]
languages =
  [ mkLang "Afrikaans"
  , mkLang "Amharic"
  , mkLang "Arabic"
  , mkLang "Bulgarian"
  , mkLang "Catalan"
  , mkLang "Chinese"
  , mkLang "Danish"
  , mkLang "Dutch"
  -- , mkLang "English"
  -- , mkLang "Estonian"
  -- , mkLang "Finnish"
  -- , mkLang "French"
  -- , mkLang "German"
  -- , mkLang "Greek"
  -- , mkLang "Hebrew"
  -- , mkLang "Hindi"
  -- , mkLang "Interlingua" `setIso` "Ina"
  -- , mkLang "Italian"
  -- , mkLang "Japanese" `setIso` "Jpn"
  -- , mkLang "Latin"
  -- , mkLang "Latvian"
  , mkLang "Maltese" `setIso` "Mlt"
  -- , mkLang "Mongolian"
  -- , mkLang "Nepali"
  -- , mkLang "Norwegian"
  -- , mkLang "Persian" `setIso` "Pes"
  -- , mkLang "Punjabi" `setIso` "Pnb"
  -- , mkLang "Polish"
  -- , mkLang "Romanian" `setIso` "Ron"
  -- , mkLang "Russian"
  -- , mkLang "Sindhi" `setIso` "Snd"
  -- , mkLang "Spanish"
  -- , mkLang "Swahili"
  -- , mkLang "Swedish"
  -- , mkLang "Thai"
  -- , mkLang "Tswana" `setIso` "Tsn"
  -- -- , mkLang "Turkish"
  -- , mkLang "Urdu"
  ]
