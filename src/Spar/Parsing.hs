{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spar.Parsing where

import qualified Data.Map as Map
import Data.XML
import Spar.Types
import qualified Text.XML as XC
import qualified Text.XML.Cursor as Cursor

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

deserializeSoapDocument :: XC.Document -> Either (Element, ParserError) SPARPersonsokningSvar
deserializeSoapDocument soapDoc =
  let [XC.NodeElement elem] = do
        envelope <- Cursor.element "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $ Cursor.fromDocument soapDoc
        body <- Cursor.element "{http://schemas.xmlsoap.org/soap/envelope/}Body" =<< Cursor.child envelope
        svar <- Cursor.element "{http://statenspersonadressregister.se/schema/personsok/2021.1/personsokningsvar}SPARPersonsokningSvar" =<< Cursor.child body
        pure $ Cursor.node svar
   in case fromElement . stripElementNamespaces . fromXmlConduitElement $ elem of
        Left e -> Left (stripElementNamespaces . fromXmlConduitElement $ elem, e)
        Right s -> pure s