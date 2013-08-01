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
    lexiconModule :: Maybe FilePath, syntaxModule :: Maybe FilePath,
    irregModule :: Maybe FilePath, dictModule :: Maybe FilePath }
  deriving (Eq,Show)

mkLang :: Text -> Lang
mkLang n = Lang n iso' (mkModule "Lexicon" iso') (mkModule "Lang" iso')
                       (mkModule "Irreg" iso') (mkModule "Dict" iso')
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
  , (mkLang "Amharic")
  , (mkLang "Arabic")
  , (mkLang "Bulgarian")
  , (mkLang "Catalan")
  , (mkLang "Chinese")
  , (mkLang "Danish")
      { syntaxModule = Nothing }
  , (mkLang "Dutch")
  , (mkLang "English")
      { syntaxModule = Nothing }
  , (mkLang "Estonian")
  , (mkLang "Finnish")
      { syntaxModule = Nothing }
  , (mkLang "French")
      { syntaxModule = Nothing }
  , (mkLang "German")
      { syntaxModule = Nothing }
  , (mkLang "Greek")
      { syntaxModule = Nothing }
  , (mkLang "Hebrew")
  , (mkLang "Hindi")
  , (mkLang "Interlingua" `setIso` "Ina")
  , (mkLang "Italian")
      { syntaxModule = Nothing }
  , (mkLang "Japanese" `setIso` "Jpn")
  -- , (mkLang "Latin")
  -- , (mkLang "Latvian" `setIso` "Lav")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Maltese" `setIso` "Mlt")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Mongolian")
  -- , (mkLang "Nepali")
  -- , (mkLang "Norwegian")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Persian" `setIso` "Pes")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Punjabi" `setIso` "Pnb")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Polish")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Romanian" `setIso` "Ron")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Russian")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Sindhi" `setIso` "Snd")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Spanish")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Swahili")
  --     { syntaxModule = Nothing }
  -- , (mkLang "Swedish")
  --     { syntaxModule = Nothing }
  , (mkLang "Thai")
      { syntaxModule = Nothing }
  , (mkLang "Tswana" `setIso` "Tsn")
      { syntaxModule = Nothing }
  , (mkLang "Turkish")
      { syntaxModule = Nothing }
  , (mkLang "Urdu")
      { syntaxModule = Nothing }
  ]
