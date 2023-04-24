-- These types encode a subset of the corresponding xml schemas.
module Spar.XML.Partial where

import Data.Text
import Data.XML
import Data.XML.Parse.Unordered
import GHC.Generics (Generic)
import Spar.XML.Full

{-
This is the top-level 'SOAP body'. Note that we don't bother with namespaces (so
they must be stripped from the document before parsing).
-}
newtype SPARPersonsokningSvarBody = SPARPersonsokningSvarBody
  { sparPersonsokningSvar :: SPARPersonsokningSvar
  }
  deriving (Generic)

instance FromElement SPARPersonsokningSvarBody where
  fromElement = p . stripAllNamespaces'
    where
      p = parseUnorderedElement $ do
        sparPersonsokningSvar <- consumeElement "SPARPersonsokningSvar"
        pure SPARPersonsokningSvarBody {..}

{-
<xs:element name="SPARPersonsokningSvar"> <xs:annotation> <xs:documentation>
    Innehåller frågan samt svarsposter utan sortering </xs:documentation>
        </xs:annotation> <xs:complexType> <xs:sequence> <xs:element
            ref="sok:PersonsokningFraga" minOccurs="0"/> <xs:choice> <xs:element
        ref="PersonsokningSvarspost" minOccurs="0" maxOccurs="unbounded"/>
    <xs:element ref="undantag:Undantag" minOccurs="0"/> <xs:element
    ref="undantag:OverstigerMaxAntalSvarsposter" minOccurs="0"/> </xs:choice>
        <xs:element name="UUID" type="xs:string" minOccurs="0"/> </xs:sequence>
            </xs:complexType> </xs:element>
-}
data SPARPersonsokningSvar = SPARPersonsokningSvar
  { personsokningFraga :: PersonsokningFraga,
    personsokningSvarspost :: [PersonsokningSvarspost],
    uuid :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement SPARPersonsokningSvar where
  fromElement = parseUnorderedElement $ do
    personsokningFraga <- consumeElement "PersonsokningFraga"
    personsokningSvarspost <- consumeElements "PersonsokningSvarspost"
    uuid <- consumeElement "UUID"
    pure SPARPersonsokningSvar {..}

{-
<xs:complexType name="PersonsokningFragaTYPE">
    <xs:choice>
        <xs:element ref="person:IdNummer"/>
        <xs:sequence>
            <xs:element ref="argument:FonetiskSokning" minOccurs="0"/>
            <xs:choice>
                <xs:element ref="argument:NamnSokArgument" minOccurs="0"/>
                <xs:sequence>
                    <xs:element ref="argument:FornamnSokArgument" minOccurs="0"/>
                    <xs:element ref="argument:MellanEfternamnSokArgument" minOccurs="0"/>
                </xs:sequence>
            </xs:choice>
            <xs:element ref="argument:UtdelningsadressSokArgument" minOccurs="0"/>
            <xs:element ref="argument:PostortSokArgument" minOccurs="0"/>
            <xs:choice>
                <xs:element ref="argument:PostNr" minOccurs="0"/>
                <xs:sequence>
                    <xs:element ref="argument:PostNrFrom"/>
                    <xs:element ref="argument:PostNrTom"/>
                </xs:sequence>
            </xs:choice>
            <xs:choice>
                <xs:element ref="argument:Fodelsedatum" minOccurs="0"/>
                <xs:sequence>
                    <xs:element ref="argument:FodelsedatumFrom"/>
                    <xs:element ref="argument:FodelsedatumTom"/>
                </xs:sequence>
            </xs:choice>
            <xs:element ref="argument:Kon" minOccurs="0"/>
            <xs:element ref="argument:LanKod" minOccurs="0"/>
            <xs:element ref="argument:KommunKod" minOccurs="0"/>
            <xs:choice>
                <xs:element ref="argument:DistriktKod" minOccurs="0"/>
                <xs:sequence>
                    <xs:element ref="argument:DistriktKodFrom"/>
                    <xs:element ref="argument:DistriktKodTom"/>
                </xs:sequence>
            </xs:choice>
        </xs:sequence>
    </xs:choice>
</xs:complexType>
-}
newtype PersonsokningFraga = PersonsokningFraga
  { idNummer :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement PersonsokningFraga where
  fromElement = parseUnorderedElement $ do
    idNummer <- consumeElement "IdNummer"
    pure PersonsokningFraga {..}

{-
<xs:element name="PersonsokningSvarspost" type="aviseringspost:AviseringPostTYPE"/>
<xs:complexType name="AviseringPostTYPE">
    <xs:annotation>
        <xs:documentation>En komplett post med personuppgifter</xs:documentation>
    </xs:annotation>
    <xs:sequence>
        <xs:element ref="person:PersonId"/>
        <xs:element ref="skydd:Sekretessmarkering"/>
        <xs:element ref="skydd:SekretessDatum" minOccurs="0"/>
        <xs:element ref="skydd:SkyddadFolkbokforing"/>
        <xs:element ref="skydd:SkyddadFolkbokforingDatum" minOccurs="0"/>
        <xs:element ref="person:SenasteAndringSPAR" minOccurs="0"/>
        <xs:element ref="inkomst:SummeradInkomst" minOccurs="0"/>
        <xs:element ref="inkomst:InkomstAr" minOccurs="0"/>
        <xs:element ref="namn:Namn" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="pdt:Persondetaljer" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="fb:Folkbokforing" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="fba:Folkbokforingsadress" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="spa:SarskildPostadress" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="ua:Utlandsadress" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="ka:Kontaktadress" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="relation:Relation" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="fastighet:Fastighet" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
</xs:complexType>
-}

data PersonsokningSvarspost = PersonsokningSvarspost
  { personId :: PersonId,
    sekretessmarkering :: SekretessmarkeringMedAttributTYPE,
    skyddadFolkbokforing :: Text,
    senasteAndringSPAR :: Maybe Text,
    namn :: Maybe Namn,
    persondetaljer :: [Persondetaljer],
    folkbokforingsadress :: [Folkbokforingsadress]
  }
  deriving stock (Generic, Show, Eq)

{-
<xs:element name="Sekretessmarkering" type="SekretessmarkeringMedAttributTYPE"/>
<xs:complexType name="SekretessmarkeringMedAttributTYPE">
    <xs:simpleContent>
        <xs:extension base="typ:JaNejTYPE">
            <xs:attribute name="sattAvSPAR" type="SekretessSattAvSPARTYPE"/>
        </xs:extension>
    </xs:simpleContent>
</xs:complexType>
-}

newtype SekretessmarkeringMedAttributTYPE = SekretessmarkeringMedAttributTYPE Text
  deriving stock (Generic, Show, Eq)

instance FromElement SekretessmarkeringMedAttributTYPE where
  fromElement =
    parseContentElementPartially
      (\t _ -> pure $ SekretessmarkeringMedAttributTYPE t)

instance FromElement PersonsokningSvarspost where
  fromElement = parseUnorderedElementPartially $ do
    personId <- consumeElement "PersonId"
    sekretessmarkering <- consumeElement "Sekretessmarkering"
    skyddadFolkbokforing <- consumeElement "SkyddadFolkbokforing"
    senasteAndringSPAR <- consumeElementOrAbsent "SenasteAndringSPAR"
    namn <- consumeElementOrAbsent "Namn"
    persondetaljer <- consumeElements "Persondetaljer"
    folkbokforingsadress <- consumeElements "Folkbokforingsadress"
    pure PersonsokningSvarspost {..}

{-
<xs:element name="Persondetaljer" type="PersondetaljerTYPE"/>
<xs:complexType name="PersondetaljerTYPE">
    <xs:annotation>
        <xs:documentation>
            Detaljerad information direkt relaterad till personen som SPAR för de uppgifter SPAR
            sparar historik. Poster som saknar elementen DatumFrom och DatumTom avser aktuell information.
        </xs:documentation>
    </xs:annotation>
    <xs:sequence>
        <xs:group ref="datumtid:DatumIntervall" minOccurs="0"/>
        <xs:element ref="skydd:Sekretessmarkering" minOccurs="0"/>
        <xs:element ref="skydd:SkyddadFolkbokforing" minOccurs="0"/>
        <xs:element ref="avreg:AvregistreringsorsakKod" minOccurs="0"/>
        <xs:element ref="avreg:Avregistreringsdatum" minOccurs="0"/>
        <xs:element ref="avreg:Avlidendatum" minOccurs="0"/>
        <xs:element ref="avreg:AntraffadDodDatum" minOccurs="0"/>
        <xs:element ref="Fodelsedatum" minOccurs="0"/>
        <xs:element ref="FodelselanKod" minOccurs="0"/>
        <xs:element ref="Fodelseforsamling" minOccurs="0"/>
        <xs:element ref="Kon" minOccurs="0"/>
        <xs:element ref="SvenskMedborgare" minOccurs="0"/>
        <xs:element ref="hanvisning:Hanvisning" minOccurs="0" maxOccurs="unbounded"/>
        <xs:element ref="sn:SnStatus" minOccurs="0"/>
        <xs:element ref="sn:SnTilldelningsdatum" minOccurs="0"/>
        <xs:element ref="sn:SnPreliminartVilandeforklaringsdatum" minOccurs="0"/>
        <xs:element ref="sn:SnFornyelsedatum" minOccurs="0"/>
        <xs:element ref="sn:SnVilandeorsak" minOccurs="0"/>
        <xs:element ref="sn:SnVilandeforklaringsdatum" minOccurs="0"/>
        <xs:element ref="sn:SnAvlidendatum" minOccurs="0"/>
    </xs:sequence>
</xs:complexType>
-}

data Persondetaljer = Persondetaljer
  { datumFrom :: Maybe SparDay,
    datumTill :: Maybe SparDay,
    sekretessmarkering :: Maybe Text,
    skyddadFolkbokforing :: Maybe Text,
    fodelsedatum :: Maybe Text,
    kon :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

instance FromElement Persondetaljer where
  fromElement = parseUnorderedElementPartially $ do
    datumFrom <- consumeElementOrAbsent "DatumFrom"
    datumTill <- consumeElementOrAbsent "DatumTill"
    sekretessmarkering <- consumeElementOrAbsent "Sekretessmarkering"
    skyddadFolkbokforing <- consumeElementOrAbsent "SkyddadFolkbokforing"
    fodelsedatum <- consumeElementOrAbsent "Fodelsedatum"
    kon <- consumeElementOrAbsent "Kon"
    pure Persondetaljer {..}
