{-# LANGUAGE TypeApplications #-}

module Spar.Types where

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import Data.XML
import GHC.Generics (Generic)
import Text.XML (Name)

type SparResponse = Either SparError PersonsokningSvarpost

type SparResponse' = Either SparError SPARPersonsokningSvar

data SparError = NoParse ParserError | RequestError Text | PersonNotFound SSN deriving (Show)

type SSN = Text

type Query = Text

type KundNrSlutKund = Text

type UppdragId = Text

data SPARPersonsokningSvar = SPARPersonsokningSvar
  { personsokningFraga :: PersonsokningFraga,
    personsokningSvarspost :: Maybe PersonsokningSvarpost,
    uuid :: Text
  }
  deriving stock (Generic, Show, Eq)

newtype SPARResponseDocument = SPARResponseDocument {personsokningSvarspost :: SPARPersonsokningSvar}
  deriving stock (Generic, Show, Eq)

instance FromElement SPARPersonsokningSvar where
  fromElement =
    parseOrderedElement $ do
      query <- consumeElement "PersonsokningFraga"
      answer <- consumeElementOrAbsent "PersonsokningSvarspost"
      uuid <- consumeElement "UUID"
      -- _ <- consumeRemainingElements
      pure (SPARPersonsokningSvar query answer uuid)

newtype PersonsokningFraga = PersonsokningFraga
  { idNummer :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement PersonsokningFraga where
  fromElement = parseOrderedElement $ do
    personId <- consumeElement "IdNummer"
    pure (PersonsokningFraga personId)

data PersonsokningSvarpost = PersonsokningSvarpost
  { personId :: PersonId,
    sekretessMarkering :: Text,
    skyddadFolkbokforing :: Text,
    senasteAndringSPAR :: Maybe Text,
    namn :: Maybe Namn,
    persondetaljer :: Maybe Persondetaljer,
    folkbokforning :: [Folkbokforing],
    folkbokforingsadress :: Maybe Folkbokforingsadress
  }
  deriving stock (Generic, Show, Eq)

instance FromElement PersonsokningSvarpost where
  fromElement = parseUnorderedElement' $ do
    el <- consumeElement "PersonId"
    sekretess <- consumeElement "Sekretessmarkering"
    skyddad <- consumeElement "SkyddadFolkbokforing"
    senaste <- consumeElementOrAbsent "SenasteAndringSPAR"
    namn <- consumeElementOrAbsent "Namn"
    persondetailer <- consumeElementOrAbsent "Persondetaljer"
    folkbokforing <- consumeElements "Folkbokforing"
    adress <- consumeElementOrAbsent "Folkbokforingsadress"
    _ <- consumeRemainingElements
    pure (PersonsokningSvarpost el sekretess skyddad senaste namn persondetailer folkbokforing adress)

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

data Namn = Namn
  { datumFrom :: Day,
    datumTill :: Day,
    fornamn :: Maybe Text,
    efternamn :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Namn where
  fromElement = parseUnorderedElement' $ do
    from <- consumeDay "DatumFrom"
    to <- consumeDay "DatumTill"
    firstName <- consumeElementOrAbsent "Fornamn"
    lastName <- consumeElement "Efternamn"
    pure (Namn from to firstName lastName)

data Persondetaljer = Persondetaljer
  { datumFrom :: Day,
    datumTill :: Day,
    sekretessmarkering :: Maybe Text,
    skyddadFolkbokforing :: Maybe Text,
    fodelsesdatum :: Maybe Text,
    kon :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Persondetaljer where
  fromElement = parseUnorderedElement' $ do
    from <- consumeDay "DatumFrom"
    to <- consumeDay "DatumTill"
    sekretessmarkering <- consumeElementOrAbsent "Sekretessmarkering"
    skyddad <- consumeElementOrAbsent "SkyddadFolkbokforing"
    date <- consumeElementOrAbsent "Fodelsedatum"
    gender <- consumeElementOrAbsent "Kon"
    pure (Persondetaljer from to sekretessmarkering skyddad date gender)

data Folkbokforing = Folkbokforing
  { datumFrom :: Day,
    datumTill :: Day,
    folkbokfordLanKod :: Text,
    folkbokfordKommunKod :: Text,
    hemvist :: Text,
    folkbokforingsdatum :: Day,
    distriktKod :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Folkbokforing where
  fromElement = parseOrderedElement $ do
    from <- consumeDay "DatumFrom"
    to <- consumeDay "DatumTill"
    lankod <- consumeElement "FolkbokfordLanKod"
    kommunkod <- consumeElement "FolkbokfordKommunKod"
    hemvist <- consumeElement "Hemvist"
    datum <- consumeDay "Folkbokforingsdatum"
    distrikt <- consumeElementOrAbsent "DistriktKod"
    pure (Folkbokforing from to lankod kommunkod hemvist datum distrikt)

newtype Folkbokforingsadress = Folkbokforingsadress
  { svenskAdress :: SvenskAdress
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Folkbokforingsadress where
  fromElement = parseUnorderedElement' $ do
    svenskAdress <- consumeElement "SvenskAdress"
    pure (Folkbokforingsadress svenskAdress)

data SvenskAdress = SvenskAdress
  { datumFrom :: Day,
    datumTill :: Day,
    utdelningsadress1 :: Maybe Text,
    utdelningsadress2 :: Maybe Text,
    postNr :: Text,
    postort :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement SvenskAdress where
  fromElement = parseUnorderedElement' $ do
    from <- consumeDay "DatumFrom"
    to <- consumeDay "DatumTill"
    utdelningsadress1 <- consumeElementOrAbsent "Utdelningsadress1"
    utdelningsadress2 <- consumeElementOrAbsent "Utdelningsadress2"
    postNr <- consumeElement "PostNr"
    postOrt <- consumeElement "Postort"
    pure (SvenskAdress from to utdelningsadress1 utdelningsadress2 postNr postOrt)

data Config = Config
  { url :: String,
    certFile :: String,
    keyFile :: String,
    customerNr :: KundNrSlutKund,
    assignmentId :: UppdragId,
    endUserId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

parseUnorderedElement' :: (forall m. (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m a) -> Element -> Either ParserError a
parseUnorderedElement' go = parseUnorderedElement $
  do
    result <- go
    _ <- consumeRemainingElements
    pure result

consumeDay :: (ElementConsumer m) => Name -> m Day
consumeDay name = do
  consumeElementOrAbsent name >>= \case
    Nothing -> throwParserError $ "Missing " <> Text.pack (show name) <> " element."
    Just (text :: Text) ->
      case parseTimeM False defaultTimeLocale "%Y-%m-%d" $ Text.unpack text of
        Nothing -> throwParserError $ "Could not parse element " <> Text.pack (show name) <> " as `Day`."
        Just day -> pure day