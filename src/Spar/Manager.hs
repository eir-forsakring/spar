{-# LANGUAGE OverloadedLabels #-}

module Spar.Manager where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BL
import Data.Default (Default (def))
import Data.Generics.Labels ()
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.Connection (TLSSettings (TLSSettings))
import Network.HTTP.Client
  ( Manager,
    Request (method, requestBody, requestHeaders),
    RequestBody (RequestBodyLBS),
    defaultManagerSettings,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Req (Scheme (Http, Https))
import Network.TLS
  ( ClientHooks (onCertificateRequest, onServerCertificate),
    ClientParams (clientHooks, clientSupported),
    Supported (supportedCiphers),
    credentialLoadX509,
    defaultParamsClient,
  )
import Network.TLS.Extra.Cipher
import Spar.Types

mkRequest :: SSN -> Text
mkRequest ssn =
  "<?xml version='1.0' ?> \
  \<soapenv:Envelope \
  \   xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/' \
  \   xmlns:per='http://statenspersonadressregister.se/schema/personsok/2021.1/personsokningfraga' \
  \   xmlns:iden='http://statenspersonadressregister.se/schema/komponent/metadata/identifieringsinformationWs-1.1' \
  \   xmlns:per1='http://statenspersonadressregister.se/schema/komponent/sok/personsokningsokparametrar-1.1' \
  \   xmlns:per2='http://statenspersonadressregister.se/schema/komponent/person/person-1.2' \
  \   xmlns:sok='http://statenspersonadressregister.se/schema/komponent/sok/sokargument-1.2'> \
  \ <soapenv:Header/> \
  \ <soapenv:Body> \
  \    <per:SPARPersonsokningFraga> \
  \       <iden:Identifieringsinformation> \
  \          <iden:KundNrLeveransMottagare>500243</iden:KundNrLeveransMottagare> \
  \          <iden:KundNrSlutkund>500243</iden:KundNrSlutkund> \
  \          <iden:UppdragId>637</iden:UppdragId> \
  \          <iden:SlutAnvandarId>Anställd X på avdelning Y, Testsökning C# .NET Core</iden:SlutAnvandarId> \
  \       </iden:Identifieringsinformation> \
  \       <per1:PersonsokningFraga> \
  \          <per2:IdNummer>"
    <> ssn
    <> "</per2:IdNummer> \
       \       </per1:PersonsokningFraga> \
       \    </per:SPARPersonsokningFraga> \
       \  </soapenv:Body> \
       \ </soapenv:Envelope>"

makeClientManager :: Scheme -> Config -> IO Manager
makeClientManager Https cfg = mkMngr cfg
makeClientManager Http _ = newManager defaultManagerSettings

makeTLSManager :: Config -> IO Manager
makeTLSManager = makeClientManager Https

mkMngr :: Config -> IO Manager
mkMngr cfg = do
  creds <- either error Just `fmap` credentialLoadX509 "/sparcert.pem" "/sparcert.pem"
  let hooks =
        def
          { onCertificateRequest = \_ -> return creds,
            onServerCertificate = \_ _ _ _ -> return []
          }
      clientParams =
        (defaultParamsClient (cfg ^. #url) "")
          { clientHooks = hooks,
            clientSupported = def {supportedCiphers = ciphersuite_default}
          }
      tlsSettings = TLSSettings clientParams

  newManager $ mkManagerSettings tlsSettings Nothing

buildRequest :: String -> Query -> IO Request
buildRequest url query = do
  nakedRequest <- parseRequest url
  return
    ( nakedRequest
        { method = "POST",
          requestHeaders =
            [ ("Content-Type", "text/xml")
            ],
          requestBody = RequestBodyLBS $ BL.fromStrict . TE.encodeUtf8 $ query
        }
    )