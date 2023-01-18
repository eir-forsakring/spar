{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Spar
import Spar.Parsing
import Spar.Types
import Test.Hspec
import TestHarness

main :: IO ()
main = do
  hspec $ spec testCfg

spec :: Config -> Spec
spec cfg = do
  describe "deserialize" $ do
    it "loads a response" $ do
      s <- liftIO $ testSSN "./test/personnummer.csv"
      n <- mapM (loadDocument "./test/responses") (take 5 s)
      print (map parseDocument n)
      1 `shouldBe` 1