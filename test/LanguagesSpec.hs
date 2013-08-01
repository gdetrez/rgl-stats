{-# LANGUAGE OverloadedStrings #-}
module LanguagesSpec where

import Languages
import Test.Hspec

spec :: Spec
spec = do
  describe "mkLang" $ do

    it "makes a new Lang with the given name" $ do
      name (mkLang "Name") `shouldBe` "Name"

    it "uses the 3 first letter as iso code" $ do
      iso (mkLang "Name") `shouldBe` "Nam"

  describe "mkModule" $ do

    it "Creates the module path" $
      mkModule "Lexicon" "Eng"
        `shouldBe` "dist/build/rgl/alltenses/LexiconEng.gfo"
