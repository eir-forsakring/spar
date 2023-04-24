-- These types directly encode their corresponding xml schema.
module Spar.XML.Full where

import Data.Text
import qualified Data.Text as Text
import Data.Time
import Data.XML
import Data.XML.Parse.Ordered
import GHC.Generics (Generic)

{-
<xs:simpleType name="SparDatumTYPE">
    <xs:annotation>
        <xs:documentation>
            Ett datum på format YYYY-MM-DD
        </xs:documentation>
    </xs:annotation>
    <xs:restriction base="xs:date">
    </xs:restriction>
</xs:simpleType>
-}
newtype SparDay = SparDay Day
  deriving stock (Show, Eq)

instance FromContent SparDay where
  fromContent text i = case parseTimeM False defaultTimeLocale "%Y-%m-%d" $ Text.unpack text of
    Just day -> pure $ SparDay day
    Nothing -> Left $ parserError i "Invalid date"

deriving via ContentElement SparDay instance FromElement SparDay

{-
<xs:element name="PersonId" type="PersonIdTYPE"/>
<xs:complexType name="PersonIdTYPE">
    <xs:sequence>
        <xs:element ref="IdNummer"/>
        <xs:element ref="Typ"/>
    </xs:sequence>
</xs:complexType>
-}
data PersonId = PersonId
  { idNummer :: Text,
    typ :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement PersonId where
  fromElement = parseOrderedElement $ do
    idNummer <- consumeElement "IdNummer"
    typ <- consumeElement "Typ"
    pure PersonId {..}

{-
<xs:element name="Namn" type="NamnTYPE"/>
<xs:complexType name="NamnTYPE">
    <xs:sequence>
        <xs:group ref="datumtid:DatumIntervall" minOccurs="0"/>
        <xs:element ref="namn:Aviseringsnamn" minOccurs="0"/>
        <xs:element ref="namn:Fornamn" minOccurs="0"/>
        <xs:element ref="namn:Tilltalsnamn" minOccurs="0"/>
        <xs:element ref="namn:Mellannamn" minOccurs="0"/>
        <xs:element ref="namn:Efternamn" minOccurs="0"/>
    </xs:sequence>
</xs:complexType>
-}
data Namn = Namn
  { datumFrom :: SparDay,
    datumTill :: SparDay,
    aviseringsnamn :: Maybe Text,
    fornamn :: Maybe Text,
    tilltalsnamn :: Maybe Text,
    mellannamn :: Maybe Text,
    efternamn :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Namn where
  fromElement = parseOrderedElement $ do
    datumFrom <- consumeElement "DatumFrom"
    datumTill <- consumeElement "DatumTill"
    aviseringsnamn <- consumeElementOrAbsent "Aviseringsnamn"
    fornamn <- consumeElementOrAbsent "Fornamn"
    tilltalsnamn <- consumeElementOrAbsent "Tilltalsnamn"
    mellannamn <- consumeElementOrAbsent "Mellannamn"
    efternamn <- consumeElementOrAbsent "Efternamn"
    pure Namn {..}

{-
<xs:group name="SvenskAdress">
    <xs:sequence>
        <xs:group ref="datumtid:DatumIntervall" minOccurs="0"/>
        <xs:element ref="CareOf" minOccurs="0"/>
        <xs:element ref="Utdelningsadress1" minOccurs="0"/>
        <xs:element ref="Utdelningsadress2" minOccurs="0"/>
        <xs:element ref="PostNr" minOccurs="0"/>
        <xs:element ref="Postort" minOccurs="0"/>
    </xs:sequence>
</xs:group>

<xs:element name="SvenskAdress" type="SvenskAdressTYPE"/>
<xs:complexType name="SvenskAdressTYPE">
    <xs:sequence>
        <xs:group ref="SvenskAdress"/>
    </xs:sequence>
</xs:complexType>
-}
data SvenskAdress = SvenskAdress
  { datumFrom :: Maybe SparDay,
    datumTill :: Maybe SparDay,
    careOf :: Maybe Text,
    utdelningsadress1 :: Maybe Text,
    utdelningsadress2 :: Maybe Text,
    postNr :: Maybe Text,
    postort :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement SvenskAdress where
  fromElement = parseOrderedElement $ do
    datumFrom <- consumeElementOrAbsent "DatumFrom"
    datumTill <- consumeElementOrAbsent "DatumTill"
    careOf <- consumeElementOrAbsent "CareOf"
    utdelningsadress1 <- consumeElementOrAbsent "Utdelningsadress1"
    utdelningsadress2 <- consumeElementOrAbsent "Utdelningsadress2"
    postNr <- consumeElementOrAbsent "PostNr"
    postort <- consumeElementOrAbsent "Postort"
    pure SvenskAdress {..}

{-
<xs:element name="Folkbokforingsadress" type="FolkbokforingsadressTYPE"/>
<xs:complexType name="FolkbokforingsadressTYPE">
    <xs:sequence>
        <xs:element ref="adress:SvenskAdress"/>
    </xs:sequence>
</xs:complexType>
-}
newtype Folkbokforingsadress = Folkbokforingsadress
  { svenskAdress :: SvenskAdress
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Folkbokforingsadress where
  fromElement = parseOrderedElement $ do
    svenskAdress <- consumeElement "SvenskAdress"
    pure Folkbokforingsadress {..}
