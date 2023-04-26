{-# LANGUAGE TypeApplications #-}

module Spar.Types
  ( Config (..),
    KundNrSlutKund,
    UppdragId,
    SparResponse,
    SparError (..),
    SSN,
    Query,
    module Spar.XML.Full,
    module Spar.XML.Partial,
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (Response)
import Spar.XML.Full
import Spar.XML.Partial

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

type KundNrSlutKund = Text

type UppdragId = Text

type SparResponse = Either SparError AviseringPostTYPE

data SparError
  = NoParse String
  | UnexpectedParseResult Text
  | RequestError (Response BL.ByteString)
  | TransportError Text
  | PersonNotFound SSN
  deriving (Show, Exception)

type SSN = Text

type Query = Text
