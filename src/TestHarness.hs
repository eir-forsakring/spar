{-# LANGUAGE OverloadedStrings #-}

module TestHarness where

import Control.Monad (forM_)
import Data.Default
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Client (Response (responseBody))
import Spar (queryWithSSN, queryWithSSNRaw)
import Spar.Parsing (deserializeSoapDocument)
import Spar.Types
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStrLn, openFile, withFile)
import qualified System.IO as IO
import qualified Text.XML as XC

testSSN :: FilePath -> IO [Text]
testSSN path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle

  return $ map T.pack $ lines contents

deserializeFile :: FilePath -> IO ()
deserializeFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle

  IO.print $ deserializeSoapDocument (XC.parseText_ def . TL.fromStrict . T.pack $ contents)

saveRawResponse :: Config -> FilePath -> SSN -> IO ()
saveRawResponse cfg path ssn = do
  let writePath = T.pack path <> "/" <> ssn <> ".xml"
  response <- queryWithSSNRaw cfg ssn
  withFile (T.unpack writePath) WriteMode $ \file -> do
    hPutStrLn file (T.unpack . TL.toStrict . TL.decodeUtf8 . responseBody $ response)

testCfg :: Config
testCfg = Config "https://kt-ext-ws.statenspersonadressregister.se/2021.1/personsok"

runTests :: IO ()
runTests = do
  testSSNs <- testSSN "./test/personnummer.csv"
  forM_
    (testSSNs & take 10)
    ( \ssn ->
        queryWithSSN cfg ssn
          >>= ( \case
                  Left e -> do
                    print ("Error calling " <> ssn <> " result:")
                    print e
                  Right r -> print r
              )
    )
  where
    cfg = Config "https://kt-ext-ws.statenspersonadressregister.se/2021.1/personsok"

testFailingRequest :: IO SparResponse
testFailingRequest = queryWithSSN cfg "201507212387"
  where
    cfg = Config "https://kt-ext-ws.statenspersonadressregister.seo/2021.1/"

testFailingPerson :: IO SparResponse
testFailingPerson = queryWithSSN cfg "201507200000"
  where
    cfg = Config "https://kt-ext-ws.statenspersonadressregister.se/2021.1/"

testRequest :: IO SparResponse
testRequest = queryWithSSN cfg "201507212387"
  where
    cfg = Config "https://kt-ext-ws.statenspersonadressregister.se/2021.1/"