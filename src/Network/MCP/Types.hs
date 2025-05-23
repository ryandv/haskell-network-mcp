{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Network.MCP.Types
  ( ClientCapabilities(..)

  , emptyObj

  , InitializeRequest(..)
  , InitializeResponse(..)
  , InitializedNotification(..)

  , Implementation(..)
  , InputSchema(..)

  , ListChangedCapability(..)
  , ListChangedAndSubscriptionCapabilities(..)

  , noCapabilities

  , ServerCapabilities(..)

  , Tool(..)
  , ToolAnnotations(..)
  ) where

import qualified Prelude

import Prelude hiding(id)

import Data.Aeson.KeyMap
import Data.Aeson.Types hiding(Result)
import Data.Kind
import Data.Text
import Data.Vector

import GHC.Generics
import GHC.Records

import Network.JSONRPC

protocolVersion :: Text
protocolVersion = "2025-03-26"

customOptions :: Options
customOptions = defaultOptions { omitNothingFields = True }

emptyObj :: Object
emptyObj = Data.Aeson.KeyMap.empty

data Tool = Tool
  { name        :: Text
  , description :: Maybe Text
  , inputSchema :: InputSchema
  , annotations :: Maybe ToolAnnotations
  } deriving(Eq, Generic, Show)

instance FromJSON Tool
instance ToJSON Tool where
  toEncoding = genericToEncoding customOptions

data ToolAnnotations = ToolAnnotations
  { title           :: Text
  , readOnlyHint    :: Maybe Bool
  , destructiveHint :: Maybe Bool
  , idempotentHint  :: Maybe Bool
  , openWorldHint   :: Maybe Bool
  } deriving(Eq, Generic, Show)

instance FromJSON ToolAnnotations
instance ToJSON ToolAnnotations where
  toEncoding = genericToEncoding customOptions

data InputSchema = InputSchema
  { properties :: Maybe Object
  , required   :: Maybe (Vector Text)
  } deriving(Eq, Generic, Show)

instance FromJSON InputSchema
instance ToJSON InputSchema where
  toJSON is = case genericToJSON customOptions is of
    (Object o) -> Object $ insert "type" "object" o
    _          -> genericToJSON customOptions is

{-----------------------------------------------------------
-- requests & responses
-----------------------------------------------------------}

data InitializeRequest = InitializeRequest
  { capabilities    :: ClientCapabilities
  , clientInfo      :: Implementation
  } deriving(Eq, Generic, Show)

instance FromRequest InitializeRequest where
  parseParams = const $ Just parseJSON

instance ToRequest InitializeRequest where
  requestMethod  = const "initialize"
  requestIsNotif = const False

instance FromResponse InitializeResponse where
  parseResult = const $ Just parseJSON

-- the hardcoded protocolVersion seems to trip up
-- aeson's generics...
instance FromJSON InitializeRequest
instance ToJSON InitializeRequest where
  toEncoding q = pairs (  "protocolVersion" .= protocolVersion
                       <> "capabilities"    .= getField @"capabilities" q
                       <> "clientInfo"      .= clientInfo q
                       )
  toJSON q = object [ "protocolVersion" .= protocolVersion
                    , "capabilities"    .= (genericToJSON customOptions (getField @"capabilities" q))
                    , "clientInfo"      .= (genericToJSON customOptions (clientInfo q))
                    ]

data InitializeResponse = InitializeResponse
  { capabilities    :: ServerCapabilities
  , serverInfo      :: Implementation
  , instructions    :: Maybe Text
  } deriving(Eq, Generic, Show)

instance FromJSON InitializeResponse
instance ToJSON InitializeResponse where
  toEncoding r = pairs (  "capabilities" .= getField @"capabilities" r
                       <> "serverInfo"   .= serverInfo r
                       <> "instructions" .= instructions r
                       )
  toJSON r = case genericToJSON customOptions r of
    (Object o) -> Object $ insert "capabilities" (genericToJSON customOptions (getField @"capabilities" r))
                         $ insert "serverInfo"   (genericToJSON customOptions (serverInfo r))
                         $ o

data InitializedNotification = InitializedNotification

instance FromJSON InitializedNotification where
  parseJSON (Object o) = o .: "method" >>= (\m -> if m == ("notifications/initialized" :: Text) then return InitializedNotification else mempty)
  parseJSON _          = mempty
instance ToJSON InitializedNotification where
  toJSON _ = object [ "method" .= ("notifications/initialized" :: Text) ]

{-----------------------------------------------------------
-- plain old data
-----------------------------------------------------------}

-- no experimental support
data ClientCapabilities = ClientCapabilities
  { roots        :: Maybe ListChangedCapability
  , sampling     :: Maybe Object
  } deriving(Eq, Generic, Show)

instance FromJSON ClientCapabilities
instance ToJSON ClientCapabilities where
  toEncoding = genericToEncoding customOptions

-- no experimental support
data ServerCapabilities = ServerCapabilities
  { logging      :: Maybe Object
  , completions  :: Maybe Object
  , prompts      :: Maybe ListChangedCapability
  , resources    :: Maybe ListChangedAndSubscriptionCapabilities
  , tools        :: Maybe ListChangedCapability
  } deriving(Eq, Generic, Show)

instance FromJSON ServerCapabilities
instance ToJSON ServerCapabilities where
  toEncoding = genericToEncoding customOptions

noCapabilities :: ServerCapabilities
noCapabilities = ServerCapabilities
  { logging     = Nothing
  , completions = Nothing
  , prompts     = Nothing
  , resources   = Nothing
  , tools       = Nothing
  }

data ListChangedCapability = ListChangedCapability
  { listChanged :: Bool
  } deriving(Eq, Generic, Show)

instance FromJSON ListChangedCapability
instance ToJSON ListChangedCapability where
  omitField = const False
  toEncoding = genericToEncoding customOptions

data ListChangedAndSubscriptionCapabilities = ListChangedAndSubscriptionCapabilities
  { listChanged :: Bool
  , subscribe   :: Bool
  } deriving(Eq, Generic, Show)

instance FromJSON ListChangedAndSubscriptionCapabilities
instance ToJSON ListChangedAndSubscriptionCapabilities where
  omitField = const False
  toEncoding = genericToEncoding customOptions

data Implementation = Implementation
  { name    :: Text
  , version :: Text
  } deriving(Eq, Generic, Show)

instance FromJSON Implementation
instance ToJSON Implementation where
  toEncoding = genericToEncoding customOptions
