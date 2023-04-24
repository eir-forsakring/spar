{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Spar
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
      forM_ s $ \ssn -> do
        resp <- loadResponse "./test/responses" ssn
        parseResponse resp `shouldSatisfy` isRight
