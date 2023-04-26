{-# LANGUAGE OverloadedLabels #-}

module Spar where

import Control.Lens ((^.))
import Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL
import Data.Text as T
import Data.XML (decodeDocumentLBS)
import Network.HTTP.Client (Response (responseBody, responseStatus), httpLbs)
import Network.HTTP.Types (statusIsSuccessful)
import Spar.Manager
import Spar.Types
import Spar.XML.SOAP

queryWithSSN :: Config -> SSN -> IO SparResponse
queryWithSSN cfg ssn = do
  response <- (Right . responseBody <$> queryWithSSNRaw cfg ssn) `catch` (return . Left)
  return $ parseResponse =<< response

queryWithSSNRaw :: Config -> SSN -> IO (Response BL.ByteString)
queryWithSSNRaw cfg ssn = do
  res <- doRequest `catch` handleError

  if statusIsSuccessful $ responseStatus res
    then pure res
    else throwM $ RequestError res
  where
    handleError :: SomeException -> IO a
    handleError e = throwM $ TransportError $ T.pack $ show e

    doRequest :: IO (Response BL.ByteString)
    doRequest = do
      manager <- makeTLSManager cfg
      request <- buildRequest (cfg ^. #url) (mkRequest cfg ssn)
      httpLbs request manager

parseResponse :: BL.ByteString -> Either SparError AviseringPostTYPE
parseResponse e = do
  body <- parseResponse' e
  case body ^. #personsokningSvarspost of
    [] -> Left $ PersonNotFound $ body ^. #personsokningFraga . #idNummer
    (personsokningSvarspost : _) -> pure personsokningSvarspost

parseResponse' :: BL.ByteString -> Either SparError SPARPersonsokningSvar
parseResponse' e = do
  case decodeDocumentLBS @(SoapDocument SPARPersonsokningSvarBody) e of
    Left err -> Left $ NoParse err
    Right soapResponse -> Right $ soapResponse ^. #envelope . #body . #sparPersonsokningSvar
