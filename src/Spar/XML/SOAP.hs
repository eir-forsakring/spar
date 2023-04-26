module Spar.XML.SOAP where

import Data.XML
import Data.XML.Parse.Ordered
import GHC.Generics (Generic)

{- A Soap 1.1 document:

<?xml version="1.0"?>
<{http://schemas.xmlsoap.org/soap/envelope/}Envelope .../>
-}
newtype SoapDocument a = SoapDocument {envelope :: SoapEnvelope a}
  deriving stock (Generic, Show, Eq)

instance FromElement a => FromDocument (SoapDocument a) where
  fromDocument =
    fromRootElement
      "{http://schemas.xmlsoap.org/soap/envelope/}Envelope"
      SoapDocument

{-
<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
  <s:Header .../>
  <s:Body .../>
</s:Envelope>
-}
data SoapEnvelope a = SoapEnvelope
  { header :: Maybe Element,
    body :: a
  }
  deriving stock (Generic, Show, Eq)

instance FromElement a => FromElement (SoapEnvelope a) where
  fromElement = parseOrderedElement $ do
    header <- consumeElementOrAbsent "{http://schemas.xmlsoap.org/soap/envelope/}Header"
    body <- consumeElement "{http://schemas.xmlsoap.org/soap/envelope/}Body"
    pure SoapEnvelope {..}
