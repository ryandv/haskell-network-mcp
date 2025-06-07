{-# OPTIONS -Wno-redundant-constraints -Wno-unused-imports #-}

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

  , JSONRPCRequest(..)

  , ListChangedCapability(..)
  , ListChangedAndSubscriptionCapabilities(..)

  , ListToolsRequest(..)
  , ListToolsResult(..)

  , MCPError(..)
  , MCPRequest(..)
  , MCPResult(..)

  , mcpErrorJSON

  , methodInitialize
  , methodToolsCall
  , methodToolsList
  , noCapabilities

  , NotificationResult(..)

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
import Data.Text
import Data.Vector

import GHC.Generics
import GHC.Records

protocolVersion :: Text
protocolVersion = "2025-03-26"

customOptions :: Options
customOptions = defaultOptions { omitNothingFields = True }

emptyObj :: Object
emptyObj = Data.Aeson.KeyMap.empty

{-----------------------------------------------------------
-- requests & responses
-----------------------------------------------------------}

class (FromJSON q, ToJSON q) => MCPRequest q where
  methodName :: q -> Text

  mcpRequestJSON       :: (GToJSON' Value Zero (Rep q), ToJSON q) => Maybe (Either Integer Text) -> q -> Value
  mcpRequestJSON rid q = object ([ "jsonrpc" .= ("2.0" :: Text)
                                 , "method"  .= methodName q
                                 ] Prelude.++ (optionalPair "params" (if toJSON q == Null then Nothing else Just (toJSON q)))
                                   Prelude.++ (optionalPair "id" (fmap (either toJSON toJSON) rid)))

class (FromJSON r, ToJSON r) => MCPResult r where
  mcpResultJSON       :: (GToJSON' Value Zero (Rep r), ToJSON r) => Maybe (Either Integer Text) -> r -> Value
  mcpResultJSON rid r = object ([ "jsonrpc" .= ("2.0" :: Text)
                                , "result"  .= toJSON r
                                ] Prelude.++ (optionalPair "id" (fmap (either toJSON toJSON) rid)))

data MCPError = MCPError
  { code       :: Integer
  , message    :: Text
  , errorData  :: Maybe Value
  } deriving(Eq, Generic, Show)

mcpErrorJSON       :: Maybe (Either Integer Text) -> MCPError -> Value
mcpErrorJSON rid e = object ([ "jsonrpc" .= ("2.0" :: Text)
                             , "error"  .= toJSON e
                             ] Prelude.++ (optionalPair "id" (fmap (either toJSON toJSON) rid)))

instance FromJSON MCPError where
  parseJSON (Object o) = MCPError <$> o .: "code" <*> o .: "message" <*> (o .: "data" <|> return Nothing)
  parseJSON _          = mempty
instance ToJSON MCPError where
  toEncoding (MCPError c m d) = pairs (  "code"    .= c
                                      <> "message" .= m
                                      <> (optionalSeries "data" d)
                                      )
  toJSON     (MCPError c m d) = object ([ "code"    .= c
                                        , "message" .= m
                                        ] Prelude.++ (optionalPair "data" d))

data JSONRPCRequest = JSONRPCRequest
  { method :: Text
  , params :: Maybe Value
  , id     :: Maybe (Either Integer Text)
  } deriving(Eq, Show)

instance FromJSON JSONRPCRequest where
  parseJSON (Object o) = (o .: "jsonrpc" >>= (\j -> (guard (j == ("2.0" :: Text))) >> JSONRPCRequest <$> o .: "method" <*> (o .: "params" <|> return Nothing) <*> ((o .: "id" >>= (return . Just . Left)) <|> (o .: "id" >>= (return . Just . Right)) <|> return Nothing))) <|> mempty
  parseJSON _          = mempty

instance ToJSON JSONRPCRequest where
  toEncoding (JSONRPCRequest m p i) = pairs (  "method"  .= m
                                            <> "jsonrpc" .= ("2.0" :: Text)
                                            <> (optionalSeries "params" p)
                                            <> (optionalSeries "id" (fmap (either (Number . fromInteger) String) $ i))
                                            )

  toJSON     (JSONRPCRequest m p i) = object ([ "method"  .= m
                                              , "jsonrpc" .= ("2.0" :: Text)
                                              ] Prelude.++ (optionalPair "params" p)
                                                Prelude.++ (optionalPair "id" (fmap (either (Number . fromInteger) String) $ i)))

-- vacuous MCPResult instance for JSON-RPC notifications, which are not responded to
data NotificationResult = NotificationResult deriving(Eq, Generic, Show)

instance FromJSON NotificationResult where
  parseJSON _ = return NotificationResult
instance ToJSON NotificationResult where
  toJSON _ = Null
instance MCPResult NotificationResult

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

instance MCPRequest InitializeRequest where
  methodName = const $ methodInitialize

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
    (Object _) -> object ([ "protocolVersion" .= protocolVersion
                          , "capabilities"    .= (genericToJSON customOptions (getField @"capabilities" r))
                          , "serverInfo"      .= (genericToJSON customOptions (serverInfo r))
                          ] Prelude.++ (optionalPair "instructions" (instructions r)))
    _          -> genericToJSON customOptions r

instance MCPResult InitializeResult

{---------------------------------------
-- InitializedNotification
---------------------------------------}

data InitializedNotification = InitializedNotification deriving(Eq, Generic, Show)

methodNotificationsInitialized :: Text
methodNotificationsInitialized = "notifications/initialized"

instance FromJSON InitializedNotification where
  parseJSON _ = return InitializedNotification
instance ToJSON InitializedNotification where
  toJSON _ = Null

instance MCPRequest InitializedNotification where
  methodName = const $ methodNotificationsInitialized

{---------------------------------------
-- ListToolsRequest
---------------------------------------}

data ListToolsRequest = ListToolsRequest deriving(Eq, Generic, Show)

methodToolsList :: Text
methodToolsList = "tools/list"

instance FromJSON ListToolsRequest where
  parseJSON _ = return ListToolsRequest
instance ToJSON ListToolsRequest where
  toJSON _ = Null

instance MCPRequest ListToolsRequest where
  methodName = const $ methodToolsList

{---------------------------------------
-- ListToolsResult
---------------------------------------}

data ListToolsResult = ListToolsResult
  { tools :: Vector Tool
  } deriving(Eq, Generic, Show)

instance FromJSON ListToolsResult
instance ToJSON ListToolsResult where
  toEncoding = genericToEncoding customOptions
  toJSON     = genericToJSON customOptions

instance MCPResult ListToolsResult

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
  toEncoding = genericToEncoding customOptions
  toJSON     = genericToJSON customOptions

instance MCPRequest CallToolRequest where
  methodName = const $ methodToolsCall

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
    (Object _) -> object ([ "content" .= getField @"content" r
                          ] Prelude.++ (optionalPair "isError" (isError r)))
    _          -> genericToJSON customOptions r

instance MCPResult CallToolResult

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
  toJSON     = genericToJSON customOptions

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
