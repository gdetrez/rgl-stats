{-# LANGUAGE OverloadedStrings #-}
module StatusSpec where

import Test.Hspec
import Shelly
import Control.Monad (liftM)
import Languages

import Status -- SUT

spec :: Spec
spec = do
  describe "getModuleStatus" $ do

    it "returns nothing if the file does not exists" $ do
      shelly (getModuleStatus "gf" (Just "test/DoNotExists.gf"))
        `shouldReturn` Left "test/DoNotExists.gf does not exists"

    it "count the total number of fun in the abstract" $ do
      shelly (getModuleStatus "gf" (Just "test/Concrete1.gf"))
        `shouldReturn` Right (0,3)

    it "count the number of missing fun in the concrete" $ do
      shelly (getModuleStatus "gf" (Just "test/Concrete2.gf"))
        `shouldReturn` Right (1,3)

  describe "getStatus" $ do

    let testLang = (mkLang "Test") { lexiconModule = Just "test/Concrete2.gf" }

    it "Keeps the language info" $ do
      liftM lang (shelly (getStatus "gf" testLang))
        `shouldReturn` testLang

    it "get the status of the lexicon " $ do
      liftM lexiconStatus (shelly (getStatus "gf" testLang))
        `shouldReturn` Right (1,3)
