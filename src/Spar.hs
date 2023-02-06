{-# LANGUAGE OverloadedLabels #-}

module Spar where

import Control.Lens ((^.))
import Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Client (Response (responseBody), httpLbs)
import Spar.Manager
import Spar.Parsing
import Spar.Types
import qualified Text.XML as XC

queryWithSSN :: Config -> SSN -> IO SparResponse
queryWithSSN cfg ssn = do
  response <- (Right . responseBody <$> queryWithSSNRaw cfg ssn) `catch` (\(e :: SomeException) -> return $ Left (RequestError $ T.pack $ show e))
  return $ parseResponse response

queryWithSSNRaw :: Config -> SSN -> IO (Response BL.ByteString)
queryWithSSNRaw cfg ssn = do
  manager <- makeTLSManager cfg
  request <- buildRequest (cfg ^. #url) (mkRequest cfg ssn)
  httpLbs request manager

parseResponse :: Either SparError BL.ByteString -> Either SparError PersonsokningSvarpost
parseResponse e = do
  doc <- XC.parseText_ def . TL.decodeUtf8 <$> e
  parseDocument doc

parseDocument :: XC.Document -> Either SparError PersonsokningSvarpost
parseDocument doc = do
  case deserializeSoapDocument doc of
    Right r -> do
      case r ^. #personsokningSvarspost of
        Nothing -> Left $ PersonNotFound $ r ^. #personsokningFraga . #idNummer
        Just p -> pure p
    Left e -> Left $ NoParse e
