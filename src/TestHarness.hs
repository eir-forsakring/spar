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
import System.Environment (getEnv)
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

loadResponse :: FilePath -> SSN -> IO Text
loadResponse path ssn = do
  let readPath = path <> "/" <> T.unpack ssn <> ".xml"
  xs <- readFile readPath
  return $ T.pack xs

loadDocument :: FilePath -> SSN -> IO XC.Document
loadDocument path ssn = do
  s <- loadResponse path ssn
  return $ XC.parseText_ def . TL.fromStrict $ s

--  hGetContents file

testCfg :: Config
testCfg = Config "https://kt-ext-ws.statenspersonadressregister.se/2021.1/personsok" "./test/testspar.pem" "./test/testspar.pem" "500243" "637" Nothing

acquireConfig :: IO Config
acquireConfig = do
  url <- getEnv "SPAR_URL"
  certFile <- getEnv "SPAR_CERT"
  keyFile <- getEnv "SPAR_KEY"
  customerNr <- T.pack <$> getEnv "SPAR_CUSTOMER_NUMBER"
  assignmentId <- T.pack <$> getEnv "SPAR_ASSIGNMENT_ID"

  return $ Config url certFile keyFile customerNr assignmentId Nothing

runTests :: IO ()
runTests = do
  testSSNs <- testSSN "./test/personnummer.csv"
  forM_
    (testSSNs & take 10)
    ( \ssn ->
        queryWithSSN testCfg ssn
          >>= ( \case
                  Left e -> do
                    print ("Error calling " <> ssn <> " result:")
                    print e
                  Right r -> print r
              )
    )

testFailingRequest :: IO SparResponse
testFailingRequest = queryWithSSN testCfg "201507212387"

testFailingPerson :: IO SparResponse
testFailingPerson = queryWithSSN testCfg "201507200000"

testRequest :: IO SparResponse
testRequest = queryWithSSN testCfg "201507212387"