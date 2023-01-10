{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TestHarness where

import Control.Monad.Except (MonadError)
import qualified Data.ByteString.Lazy as BL
import Data.Default
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time (Day)
import Data.XML
import GHC.Generics
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Req (Scheme (Http, Https))
import Network.TLS
import Network.TLS.Extra.Cipher
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import qualified System.IO as IO
import Text.XML (def)
import qualified Text.XML as XC
import qualified Text.XML.Cursor as Cursor

testRequest :: Text
testRequest =
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
  \          <per2:IdNummer>198610279880</per2:IdNummer> \
  \       </per1:PersonsokningFraga> \
  \    </per:SPARPersonsokningFraga> \
  \  </soapenv:Body> \
  \ </soapenv:Envelope>"

data PersonsokningSvarpost = PersonsokningSvarpost
  { personId :: PersonId,
    sekretessMarkering :: Text,
    skyddadFolkbokforing :: Text,
    senasteAndringSPAR :: Text,
    namn :: Namn,
    persondetaljer :: Persondetaljer
  }
  deriving stock (Generic, Show, Eq)

-- deriving (FromElement) via PersonsokningSvarpost

instance FromElement PersonsokningSvarpost where
  fromElement = parseUnorderedElement' $ do
    el <- consumeElement "PersonId"
    sekretess <- consumeElement "Sekretessmarkering"
    skyddad <- consumeElement "SkyddadFolkbokforing"
    senaste <- consumeElement "SenasteAndringSPAR"
    namn <- consumeElement "Namn"
    persondetailer <- consumeElement "Persondetaljer"
    _ <- consumeRemainingElements
    pure (PersonsokningSvarpost el sekretess skyddad senaste namn persondetailer)

-- throwParserError (T.pack . show $ e)

newtype PersonsokningFraga = PersonsokningFraga
  { idNummer :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement PersonsokningFraga where
  fromElement = parseOrderedElement $ do
    personId <- consumeElement "IdNummer"
    pure (PersonsokningFraga personId)

data Persondetaljer = Persondetaljer
  { datumFrom :: Text,
    datumTill :: Text,
    sekretessmarkering :: Text,
    skyddadFolkbokforing :: Text,
    fodelsesdatum :: Text,
    kon :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Persondetaljer where
  fromElement = parseUnorderedElement' $ do
    from <- consumeElement "DatumFrom"
    to <- consumeElement "DatumTill"
    sekretessmarkering <- consumeElement "Sekretessmarkering"
    skyddad <- consumeElement "SkyddadFolkbokforing"
    date <- consumeElement "Fodelsedatum"
    gender <- consumeElement "Kon"
    pure (Persondetaljer from to sekretessmarkering skyddad date gender)

data Namn = Namn
  { datumFrom :: Text,
    datumTill :: Text,
    fornamn :: Text,
    efternamn :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Namn where
  fromElement = parseUnorderedElement' $ do
    from <- consumeElement "DatumFrom"
    to <- consumeElement "DatumTill"
    firstName <- consumeElement "Fornamn"
    lastName <- consumeElement "Efternamn"
    pure (Namn from to firstName lastName)

data PersonId = PersonId
  { idNummer :: Text,
    typ :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement PersonId where
  fromElement = parseUnorderedElement' $ do
    idNummer <- consumeElement "IdNummer"
    typ <- consumeElement "Typ"
    pure (PersonId idNummer typ)

newtype Sekretessmarkering = Sekretessmarkering Text deriving stock (Generic, Show)

instance FromElement Sekretessmarkering where
  fromElement = parseUnorderedElement' $ do
    sekretess <- consumeElement "Sekretessmarkering"
    pure (Sekretessmarkering sekretess)

data IdNummer = IdNummer
  { idNummer :: Text,
    typ :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement IdNummer where
  fromElement e = throwParserError (T.pack . show $ e)

-- throwParserError (T.pack . show $ e)
-- fromElement = parseUnorderedElement' $ do
--  idNummer <- consumeElement "IdNummer"
-- throwParserError "asdf"

-- _ <- consumeRemainingElements
-- typ <- consumeElementOrEmpty "Typ"
-- pure (IdNummer idNummer Nothing)

data SPARPersonsokningSvar = SPARPersonsokningSvar
  { personsokningFraga :: PersonsokningFraga,
    personsokningSvarspost :: PersonsokningSvarpost
  }
  deriving stock (Generic, Show, Eq)

newtype SPARResponseDocument = SPARResponseDocument {personsokningSvarspost :: SPARPersonsokningSvar}
  deriving stock (Generic, Show, Eq)

instance FromElement SPARPersonsokningSvar where
  fromElement =
    parseUnorderedElement' $ do
      query <- consumeElement "PersonsokningFraga"
      answer <- consumeElement "PersonsokningSvarspost"
      -- throwParserError (T.pack . show $ e)
      pure (SPARPersonsokningSvar query answer)

-- _ <- consumeRemainingElements
-- pure (SPARPersonsokningSvar el)

parseUnorderedElement' :: (forall m. (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m a) -> Element -> Either ParserError a
parseUnorderedElement' go = parseUnorderedElement $
  do
    result <- go
    _ <- consumeRemainingElements
    pure result

stripElementNamespaces :: Element -> Element
stripElementNamespaces Element {..} =
  Element
    { attributes = Map.mapKeys stripNamespace attributes,
      children = map stripNodeNamespaces children
    }

stripNodeNamespaces :: Node -> Node
stripNodeNamespaces = \case
  NodeElement name element ->
    NodeElement (stripNamespace name) (stripElementNamespaces element)
  NodeContent content -> NodeContent content

stripNamespace :: XC.Name -> XC.Name
stripNamespace name = name {XC.nameNamespace = Nothing}

deserialize :: IO ()
deserialize = do
  handle <- openFile "./test/sparresponse.xml" ReadMode
  contents <- hGetContents handle
  let soapDoc = XC.parseText_ def (TL.pack contents)
      [XC.NodeElement elem] = do
        envelope <- Cursor.element "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $ Cursor.fromDocument soapDoc
        body <- Cursor.element "{http://schemas.xmlsoap.org/soap/envelope/}Body" =<< Cursor.child envelope
        svar <- Cursor.element "{http://statenspersonadressregister.se/schema/personsok/2021.1/personsokningsvar}SPARPersonsokningSvar" =<< Cursor.child body
        -- Cursor.content =<< Cursor.child body

        pure $ Cursor.node svar
  -- IO.print $ stripElementNamespaces . fromXmlConduitElement $ elem
  IO.print (parseThing . stripElementNamespaces . fromXmlConduitElement $ elem)

-- let responseDoc' = XC.parseText_ def $ TL.fromStrict root

-- IO.print root
-- let [XC.NodeElement svarElement] = do
--      responsePost <- Cursor.element "PersonsokningSvarspost" $ Cursor.fromDocument responseDoc'
--      pure $ Cursor.node responsePost

-- [XC.NodeElement elem] = do
-- svar <- Cursor.element "{http://statenspersonadressregister.se/schema/personsok/2021.1/personsokningsvar}SPARPersonsokningSvar" =<< Cursor.child xmlDoc
-- doc <- Cursor.element =<< Cursor.fromDocument xmlDoc
--  pure $ Cursor.node doc
-- IO.print svarElement

-- IO.print (parseThing . stripElementNamespaces . fromXmlConduitElement $ elem)

runRequest :: IO ()
runRequest = do
  let hostName = "https://kt-ext-ws.statenspersonadressregister.se/2021.1/personsok"
  manager <- makeClientManager hostName Https -- _tlsManagerSettings
  request <- buildRequest "https://kt-ext-ws.statenspersonadressregister.se/2021.1/personsok"
  response <- responseBody <$> httpLbs request manager

  let soapDoc = XC.parseText_ def $ TL.decodeUtf8 response
      [XC.NodeElement elem] = do
        envelope <- Cursor.element "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $ Cursor.fromDocument soapDoc
        body <- Cursor.element "{http://schemas.xmlsoap.org/soap/envelope/}Body" =<< Cursor.child envelope
        svar <- Cursor.element "{http://statenspersonadressregister.se/schema/personsok/2021.1/personsokningsvar}SPARPersonsokningSvar" =<< Cursor.child body
        pure $ Cursor.node body
  IO.print response
  IO.print (stripElementNamespaces . fromXmlConduitElement $ elem)
  IO.print (parseThing . stripElementNamespaces . fromXmlConduitElement $ elem)

-- IO.print $ fromXmlConduitElement elem

-- IO.print $ parseThing . fromXmlConduitElement $ elem

parseThing :: Element -> Either ParserError SPARPersonsokningSvar
parseThing responseData = do
  m <- fromElement responseData
  return m

-- IO.print soapDoc

-- let doc = parseDocument (_ response)
-- IO.print doc
-- IO.print . show $ response

-- print testRequest

makeClientManager :: String -> Scheme -> IO Manager
makeClientManager hostname Https = mkMngr hostname "/home/oddvar/repos/eircorp/spar/test/testspar.pem" "/home/oddvar/repos/eircorp/spar/test/testspar.pem"
makeClientManager _ Http = newManager defaultManagerSettings

mkMngr :: String -> FilePath -> FilePath -> IO Manager
mkMngr hostName crtFile keyFile = do
  creds <- either error Just `fmap` credentialLoadX509 crtFile keyFile
  let hooks =
        def
          { onCertificateRequest = \_ -> return creds,
            onServerCertificate = \_ _ _ _ -> return []
          }
      clientParams =
        (defaultParamsClient hostName "")
          { clientHooks = hooks,
            clientSupported = def {supportedCiphers = ciphersuite_default}
          }
      tlsSettings = TLSSettings clientParams

  newManager $ mkManagerSettings tlsSettings Nothing

buildRequest :: Text -> IO Request
buildRequest url = do
  nakedRequest <- parseRequest $ T.unpack url
  return
    ( nakedRequest
        { method = "POST",
          requestHeaders =
            [ ("Content-Type", "text/xml")
            ],
          requestBody = RequestBodyLBS $ BL.fromStrict . TE.encodeUtf8 $ testRequest
        }
    )
