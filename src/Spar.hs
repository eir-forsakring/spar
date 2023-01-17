{-# LANGUAGE OverloadedLabels #-}

module Spar where

import Control.Lens ((^.))
import Data.Default (def)
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Client (Response (responseBody), httpLbs)
import Spar.Manager
import Spar.Parsing
import Spar.Types
import qualified Text.XML as XC

queryWithSSN :: Config -> SSN -> IO SparResponse
queryWithSSN cfg ssn = do
  manager <- makeTLSManager cfg
  request <- buildRequest (cfg ^. #url) (mkRequest ssn)
  response <- responseBody <$> httpLbs request manager
  let soapDoc = XC.parseText_ def $ TL.decodeUtf8 response
  case deserializeSoapDocument soapDoc of
    Left err -> do
      return $ Left $ snd err
    Right r -> return $ Right r