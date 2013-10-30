{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Languages where

import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Shelly (FilePath,(<.>),(</>))

import qualified Data.Text as T

import Predictability (Experiment(..), esc)

default (T.Text)

data Lang = Lang { name :: Text, iso :: Text,
    lexiconModule :: Maybe FilePath, syntaxModule :: Maybe FilePath,
    irregModule :: Maybe FilePath, dictModule :: Maybe FilePath,
    predictability :: [Experiment] }

mkLang :: Text -> Lang
mkLang n = Lang n iso' (mkModule "Lexicon" iso') (mkModule "Lang" iso')
                       (mkModule "Irreg" iso') (mkModule "Dict" iso')
                       []
  where iso' = T.take 3 n

setIso :: Lang -> Text -> Lang
setIso l newIso = l { iso = newIso,
    lexiconModule = mkModule "Lexicon" newIso,
    syntaxModule  = mkModule "Lang" newIso,
    irregModule   = mkModule "Irreg" newIso,
    dictModule    = mkModule "Dict" newIso }
  where oldIso = iso l

mkModule :: Text -> Text -> Maybe FilePath
mkModule prefix code = Just
  ("dist"</>"build"</>"rgl"</>"alltenses"</>(prefix <> code)<.>"gfo")

languages :: [Lang]
languages =
  [ mkLang "Afrikaans"
  , mkLang "Amharic"
  , mkLang "Arabic"
  , (mkLang "Bulgarian")
      { dictModule = Just "lib/src/bulgarian/DictBul.gf" }
  , mkLang "Catalan"
  , mkLang "Chinese"
  , mkLang "Danish"
  , mkLang "Dutch"
  , (mkLang "English")
      { dictModule = Just "lib/src/english/DictEng.gf" }
  , (mkLang "Estonian")
      { dictModule = Just "lib/src/estonian/DictEst.gf" }
  , (mkLang "Finnish")
      { dictModule = Just "lib/src/finnish/DictFin.gf" }
  , (mkLang "French")
      { dictModule = Just "lib/src/french/DictFre.gf" }
  , (mkLang "German")
      { dictModule = Just "lib/src/german/DictGer.gf" }
  , mkLang "Greek"
  , mkLang "Hebrew"
  , mkLang "Hindi"
  , mkLang "Interlingua" `setIso` "Ina"
  , mkLang "Italian"
  , mkLang "Japanese" `setIso` "Jpn"
  , mkLang "Latin"
  , mkLang "Latvian" `setIso` "Lav"
  , mkLang "Maltese" `setIso` "Mlt"
  , mkLang "Mongolian"
  , mkLang "Nepali"
  , mkLang "Norwegian"
  , mkLang "Persian" `setIso` "Pes"
  , mkLang "Punjabi" `setIso` "Pnb"
  , mkLang "Polish"
  , mkLang "Romanian" `setIso` "Ron"
  , (mkLang "Russian")
      { dictModule = Just "lib/src/russian/DictRus.gf" }
  , mkLang "Sindhi" `setIso` "Snd"
  , mkLang "Spanish"
  , mkLang "Swahili"
  , (mkLang "Swedish")
      { dictModule = Just "lib/src/swedish/DictSwe.gf" }
  , mkLang "Thai"
  , mkLang "Tswana" `setIso` "Tsn"
  , (mkLang "Turkish")
      { dictModule = Just "lib/src/turkish/DictTur.gf" }
  , (mkLang "Urdu")
      { dictModule = Just "lib/src/urdu/DictUrd.gf" }
  ]
