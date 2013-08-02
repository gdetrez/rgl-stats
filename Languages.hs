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
  [ (mkLang "Afrikaans")
  -- , (mkLang "Amharic")
  -- , (mkLang "Arabic")
  -- , (mkLang "Bulgarian")
  -- , (mkLang "Catalan")
  -- , (mkLang "Chinese")
  -- , (mkLang "Danish")
  -- , (mkLang "Dutch")
  , (mkLang "English")
      { dictModule = Just "lib/src/english/DictEng.gf" }
  -- , (mkLang "Estonian")
  -- , (mkLang "Finnish")
  -- , (mkLang "French")
  --     { predictability =
  --       [ Experiment
  --         { title         = "French verbs"
  --         , lexicon       = "lib/src/french/DictFre.gf"
  --         , category      = "V"
  --         , nforms        = 51
  --         , morphology    = "lib/src/french/ParadigmsFre.gf"
  --         , smartparadigm = "mkV"
  --         , setup         = \vForms ->
  --             let jeter = vForms!!0
  --                 jette = vForms!!3
  --                 jettera = vForms!!33
  --                 tenir = vForms!!0
  --                 tiens = vForms!!1
  --                 tient = vForms!!3
  --                 tenons = vForms!!4
  --                 tenez = vForms!!5
  --                 tiennent = vForms!!6
  --                 tienne = vForms!!7
  --                 tenions = vForms!!10
  --                 tiensI = vForms!!43
  --                 tint = vForms!!27
  --                 tiendra = vForms!!33
  --                 tenu = vForms!!46
  --             in map (map esc )
  --               [ [ jeter ]
  --               , [ jeter, jette, jettera ]
  --               , [ tenir, tiens, tenons, tiennent, tint, tiendra, tenu ]
  --               , [ tenir,tiens,tient,tenons,tenez,tiennent,tienne,tenions,tiensI,tint,tiendra,tenu ]
  --               ] }
  --     ]}
  -- , (mkLang "German")
  -- , (mkLang "Greek")
  -- , (mkLang "Hebrew")
  -- , (mkLang "Hindi")
  -- , (mkLang "Interlingua" `setIso` "Ina")
  -- , (mkLang "Italian")
  -- , (mkLang "Japanese" `setIso` "Jpn")
  -- , (mkLang "Latin")
  -- , (mkLang "Latvian" `setIso` "Lav")
  -- , (mkLang "Maltese" `setIso` "Mlt")
  -- , (mkLang "Mongolian")
  -- , (mkLang "Nepali")
  -- , (mkLang "Norwegian")
  -- , (mkLang "Persian" `setIso` "Pes")
  -- , (mkLang "Punjabi" `setIso` "Pnb")
  -- , (mkLang "Polish")
  -- , (mkLang "Romanian" `setIso` "Ron")
  -- , (mkLang "Russian")
  -- , (mkLang "Sindhi" `setIso` "Snd")
  -- , (mkLang "Spanish")
  -- , (mkLang "Swahili")
  -- , (mkLang "Swedish")
  --     { predictability =
  --       [ Experiment
  --         { title = "Swedish nouns"
  --         , lexicon = "lib/src/swedish/DictSwe.gf"
  --         , category = "N"
  --         , nforms        = 8
  --         , morphology = "lib/src/swedish/ParadigmsSwe.gf"
  --         , smartparadigm = "mkN"
  --         , setup = \(gurka:gurkas:gurkan:gurkans:gurkor:gurkors:gurkorna:gurkornas:_) ->
  --             [ [ esc gurka ]
  --             , [ esc gurka, esc gurkor ]
  --             , [ esc gurka, esc gurkan , esc gurkor, esc gurkorna ] ] }
  --       , Experiment
  --         { title = "Swedish verbs"
  --         , lexicon = "lib/src/swedish/DictSwe.gf"
  --         , category = "V"
  --         , nforms        = 17
  --         , morphology = "lib/src/swedish/ParadigmsSwe.gf"
  --         , smartparadigm = "mkV"
  --         , setup = \(gaar:gaas:gick:gicks:gaa_imp:gaa_inf:gaas_inf:gaatt:gaatts:gaangen:gaangens:gaanget:gaangets:gaangna_undef:gaangnas_undef:gaangna_def:gaangnas_def:_) ->
  --             [ [ esc gaar ]
  --             , [ esc gaa_inf, esc gick, esc gaatt ]
  --             , [ esc gaa_inf, esc gaar, esc gaa_imp, esc gick, esc gaatt, esc gaangen ] ] }
  --     ]}
  -- , (mkLang "Thai")
  -- , (mkLang "Tswana" `setIso` "Tsn")
  -- , (mkLang "Turkish")
  -- , (mkLang "Urdu")
  ]
