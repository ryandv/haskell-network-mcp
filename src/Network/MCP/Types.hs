{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Network.MCP.Types
  ( Annotations(..)

  , CallToolRequest(..)
  , CallToolResult(..)
  , CallToolResultContent(..)
  , ClientCapabilities(..)

  , emptyObj

  , InitializeRequest(..)
  , InitializeResult(..)
  , InitializedNotification(..)

  , Implementation(..)
  , InputSchema(..)

  , ListChangedCapability(..)
  , ListChangedAndSubscriptionCapabilities(..)

  , ListToolsRequest(..)
  , ListToolsResult(..)

  , methodInitialize
  , methodToolsCall
  , methodToolsList
  , noCapabilities

  , Role(..)

  , ServerCapabilities(..)

  , Tool(..)
  , ToolAnnotations(..)
  ) where

import qualified Prelude

import Prelude hiding(id)

import Control.Applicative
import Control.Monad

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

{-----------------------------------------------------------
-- requests & responses
-----------------------------------------------------------}

{---------------------------------------
-- InitializeRequest
---------------------------------------}

data InitializeRequest = InitializeRequest
  { capabilities    :: ClientCapabilities
  , clientInfo      :: Implementation
  } deriving(Eq, Generic, Show)

methodInitialize :: Text
methodInitialize = "initialize"

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

instance FromRequest InitializeRequest where
  parseParams methodInitialize = Just (genericParseJSON customOptions)
  parseParams _                = Nothing

instance ToRequest InitializeRequest where
  requestMethod  = const methodInitialize
  requestIsNotif = const False

{---------------------------------------
-- InitializeResult
---------------------------------------}

data InitializeResult = InitializeResult
  { capabilities    :: ServerCapabilities
  , serverInfo      :: Implementation
  , instructions    :: Maybe Text
  } deriving(Eq, Generic, Show)

instance FromJSON InitializeResult
instance ToJSON InitializeResult where
  toEncoding r = pairs (  "protocolVersion" .= protocolVersion
                       <> "capabilities"    .= getField @"capabilities" r
                       <> "serverInfo"      .= serverInfo r
                       <> (optionalSeries "instructions" $ instructions r)
                       )
  toJSON r = case genericToJSON customOptions r of
    (Object o) -> object ([ "protocolVersion" .= protocolVersion
                          , "capabilities"    .= (genericToJSON customOptions (getField @"capabilities" r))
                          , "serverInfo"      .= (genericToJSON customOptions (serverInfo r))
                          ] Prelude.++ (optionalPair "instructions" (instructions r)))
    _          -> genericToJSON customOptions r

instance FromResponse InitializeResult where
  parseResult = const $ Just (genericParseJSON customOptions)

{---------------------------------------
-- InitializedNotification
---------------------------------------}

data InitializedNotification = InitializedNotification deriving(Eq, Generic, Show)

methodNotificationsInitialized :: Text
methodNotificationsInitialized = "notifications/initialized"

instance FromRequest InitializedNotification where
  parseParams methodNotificationsInitialized = Just (genericParseJSON customOptions)
  parseParams _                              = Nothing

instance ToRequest InitializedNotification where
  requestMethod  = const methodNotificationsInitialized
  requestIsNotif = const True

instance FromJSON InitializedNotification where
  parseJSON Null       = return InitializedNotification
  parseJSON _          = mempty
instance ToJSON InitializedNotification where
  toJSON _ = Null

{---------------------------------------
-- ListToolsRequest
---------------------------------------}

data ListToolsRequest = ListToolsRequest deriving(Eq, Generic, Show)

methodToolsList :: Text
methodToolsList = "tools/list"

instance FromRequest ListToolsRequest where
  parseParams methodToolsList = Just parseJSON
  parseParams _               = Just parseJSON

instance ToRequest ListToolsRequest where
  requestMethod  = const methodToolsList
  requestIsNotif = const False

instance FromJSON ListToolsRequest where
  parseJSON Null       = return ListToolsRequest
  parseJSON _          = mempty
instance ToJSON ListToolsRequest where
  toJSON _ = Null

{---------------------------------------
-- ListToolsResult
---------------------------------------}

data ListToolsResult = ListToolsResult
  { tools :: Vector Tool
  } deriving(Eq, Generic, Show)

instance FromJSON ListToolsResult
instance ToJSON ListToolsResult where
  toEncoding r = pairs ( "tools" .= getField @"tools" r )
  toJSON r = case genericToJSON customOptions r of
    (Object o) -> Object $ insert "tools" (Array $ genericToJSON customOptions <$> (getField @"tools" r))
                         $ o
    _          -> genericToJSON customOptions r

instance FromResponse ListToolsResult where
  parseResult = const $ Just (genericParseJSON customOptions)

{---------------------------------------
-- CallToolRequest
---------------------------------------}

data CallToolRequest = CallToolRequest
  { name      :: Text
  , arguments :: Maybe Object
  } deriving(Eq, Generic, Show)

methodToolsCall :: Text
methodToolsCall = "tools/call"

instance FromJSON CallToolRequest where
  parseJSON (Object o) = CallToolRequest <$> o .: "name" <*> (o .: "arguments" <|> return Nothing)
  parseJSON _          = mempty
instance ToJSON CallToolRequest where
  toEncoding r = pairs (  "name"         .= getField @"name" r
                       <> (optionalSeries "arguments" $ arguments r)
                       )
  toJSON q = object ([ "name"            .= getField @"name" q
                     ] Prelude.++ (optionalPair "arguments" (arguments q)))

instance FromRequest CallToolRequest where
  parseParams methodToolsCall = Just parseJSON
  parseParams _               = Nothing

instance ToRequest CallToolRequest where
  requestMethod  = const methodToolsCall
  requestIsNotif = const False

{---------------------------------------
-- CallToolResult
---------------------------------------}

data EmbeddedResourceContents = TextResourceContents Text | BlobResourceContents Text deriving(Eq, Generic, Show)

instance FromJSON EmbeddedResourceContents where
  parseJSON (Object o) | member "text" o = TextResourceContents <$> o .: "text"
                       | member "blob" o = BlobResourceContents <$> o .: "blob"
                       | otherwise = mempty
  parseJSON _ = mempty

instance ToJSON EmbeddedResourceContents where
  toJSON (TextResourceContents t) = object [ "text" .= t ]
  toJSON (BlobResourceContents b) = object [ "blob" .= b ]

data CallToolResultContent =
  TextContent Text (Maybe Annotations)
  | ImageContent Text Text (Maybe Annotations)
  | AudioContent Text Text (Maybe Annotations)
  | EmbeddedResource EmbeddedResourceContents (Maybe Annotations) deriving(Eq, Generic, Show)

-- horrendous
instance FromJSON CallToolResultContent where
  parseJSON (Object o) =
    (o .: "type" >>=     (\t -> (guard $ (t == ("text" :: Text)))     >> TextContent      <$> o .: "text"     <*>                     (o .: "annotations" <|> return Nothing)))
    <|> (o .: "type" >>= (\t -> (guard $ (t == ("image" :: Text)))    >> ImageContent     <$> o .: "data"     <*> o .: "mimeType" <*> (o .: "annotations" <|> return Nothing)))
    <|> (o .: "type" >>= (\t -> (guard $ (t == ("audio" :: Text)))    >> AudioContent     <$> o .: "data"     <*> o .: "mimeType" <*> (o .: "annotations" <|> return Nothing)))
    <|> (o .: "type" >>= (\t -> (guard $ (t == ("resource" :: Text))) >> EmbeddedResource <$> o .: "resource" <*>                     (o .: "annotations" <|> return Nothing)))
  parseJSON _          = mempty

instance ToJSON CallToolResultContent where
  toEncoding (TextContent txt as)         = pairs (  "type"        .= ("text" :: Text)
                                                  <> "text"        .= txt
                                                  <> (optionalSeries "annotations" as)
                                                  )
  toEncoding (ImageContent d mimeType as) = pairs (  "type"        .= ("image" :: Text)
                                                  <> "data"        .= d
                                                  <> "mimeType"    .= mimeType
                                                  <> (optionalSeries "annotations" as)
                                                  )
  toEncoding (AudioContent d mimeType as) = pairs (  "type"        .= ("audio" :: Text)
                                                  <> "data"        .= d
                                                  <> "mimeType"    .= mimeType
                                                  <> (optionalSeries "annotations" as)
                                                  )
  toEncoding (EmbeddedResource c as)      = pairs (  "type"        .= ("resource" :: Text)
                                                  <> "resource"    .= c
                                                  <> (optionalSeries "annotations" as)
                                                  )

  toJSON (TextContent txt as)         = object ([ "type"        .= ("text" :: Text)
                                                , "text"        .= txt
                                                ] Prelude.++ (optionalPair "annotations" as))
  toJSON (ImageContent d mimeType as) = object ([ "type"        .= ("image" :: Text)
                                                , "data"        .= d
                                                , "mimeType"    .= mimeType
                                                ] Prelude.++ (optionalPair "annotations" as))
  toJSON (AudioContent d mimeType as) = object ([ "type"        .= ("audio" :: Text)
                                                , "data"        .= d
                                                , "mimeType"    .= mimeType
                                                ] Prelude.++ (optionalPair "annotations" as))
  toJSON (EmbeddedResource c as)      = object ([ "type"        .= ("resource" :: Text)
                                                , "resource"    .= c
                                                ] Prelude.++ (optionalPair "annotations" as))
data CallToolResult = CallToolResult
  { content         :: Vector CallToolResultContent
  , isError         :: Maybe Bool
  } deriving(Eq, Generic, Show)

instance FromJSON CallToolResult
instance ToJSON CallToolResult where
  toEncoding r = pairs (  "content"    .= getField @"content" r
                       <> (optionalSeries "isError" $ isError r)
                       )
  toJSON r = case genericToJSON customOptions r of
    (Object o) -> object ([ "content" .= getField @"content" r
                          ] Prelude.++ (optionalPair "isError" (isError r)))
    _          -> genericToJSON customOptions r

instance FromResponse CallToolResult where
  parseResult = const $ Just (genericParseJSON customOptions)

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

data Role = User | Assistant deriving(Eq, Generic, Show)

instance FromJSON Role where
  parseJSON (String "user")      = return User
  parseJSON (String "assistant") = return Assistant
  parseJSON _ = mempty
instance ToJSON Role where
  toJSON User = String "user"
  toJSON _    = String "assistant"

data Annotations = Annotations
  { audience :: Maybe (Vector Role)
  , priority :: Maybe Int
  } deriving(Eq, Generic, Show)

instance FromJSON Annotations
instance ToJSON Annotations where
  toEncoding = genericToEncoding customOptions

{-----------------------------------------------------------
-- utilities
-----------------------------------------------------------}

optionalSeries :: (ToJSON a) => Key -> Maybe a -> Series
optionalSeries = maybe mempty . (.=)

optionalPair      :: (ToJSON a) => Key -> Maybe a -> [Pair]
optionalPair k mv = maybe [] (\v -> [k .= v]) mv
