{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Server
  ( MCPT
  , ToolError(..)
  , ToolArgumentDescriptor(..)

  , defaultRequestHandlers

  , handleToolCall

  , logServerDebug
  , logServerError
  , logServerInfo
  , logServerWarn

  , server
  , stdioServer
  , toolBuilder
  ) where

import qualified Prelude
import Prelude hiding(foldr, id, lookup)

import Conduit

import Control.Concurrent.STM.TVar

import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Reader
import Control.Monad.Logger

import GHC.Generics
import GHC.Records

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Aeson.Key
import Data.ByteString(ByteString, empty)
import Data.Conduit.Combinators hiding(find, null, print)
import Data.Text hiding(empty, find, foldr, null)
import Data.Text.Lazy hiding(append, empty, find, foldr, null, pack, Text, unpack)
import Data.Vector hiding(empty, null)
import qualified Data.Aeson.KeyMap as M
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C

import Network.MCP.Types

import qualified System.IO as SIO

data ToolCallHandler m  = ToolCallHandler
  { tool    :: Tool
  , handler :: CallToolRequest -> MCPT m (Either MCPError CallToolResult)
  }

data ServerState = ServerStart | ServerInitializing | ServerOperational deriving(Eq, Show)

data ServerContext m = ServerContext
  { currentState           :: ServerState
  , clientCapabilities     :: Maybe ClientCapabilities
  , serverCapabilities     :: ServerCapabilities
  , serverTools            :: Vector (ToolCallHandler m)
  }

-- TODO: tool annotations
data ToolArgumentDescriptor = ToolArgumentDescriptor
  { argName        :: Text
  , argDescription :: Text
  , argType        :: Text
  , argRequired    :: Bool
  } deriving(Eq, Show)

data ToolError = ArgumentError Text | ExecutionError Text

type MCPT m = ReaderT (TVar (ServerContext m)) m
data RequestHandler m = forall q r. (GToJSON' Value Zero (Rep r), MCPRequest q, MCPResult r, MonadLoggerIO m ) => RequestHandler
  { requestMethod  :: Text
  , requestHandler :: q -> MCPT m (Maybe (Either MCPError r))
  }

-- TODO: tool annotations
toolBuilder            :: (Monad m)
                       => Text
                       -> Text
                       -> [ToolArgumentDescriptor]
                       -> (CallToolRequest -> MCPT m (Either ToolError CallToolResult))
                       -> ToolCallHandler m
toolBuilder n d args h = ToolCallHandler tl (h >=> either (return . mapError) (return . Right))
  where tl   = Tool n (Just d) is Nothing
        is   = InputSchema (Just . M.fromList
                                 . fmap (\a -> (fromText (argName a), object [ ("type" .= argType a), ("description" .= argDescription a) ]))
                                 $ args)
                           (Just $ fromList reqd)
        reqd = argName <$> Prelude.filter argRequired args
        mapError (ExecutionError e) = Right . flip CallToolResult (Just True) . fromList . return . flip TextContent Nothing $ e
        mapError (ArgumentError e ) = Left $ MCPError (-32602) ("Missing argument: " `append` e) Nothing

initialState                :: [ToolCallHandler m] -> ServerContext m
initialState ts | null ts   = ServerContext ServerStart Nothing noCapabilities (fromList ts)
                | otherwise = ServerContext ServerStart Nothing (noCapabilities { tools = Just $ ListChangedCapability False }) (fromList ts)

defaultRequestHandlers :: (MonadLoggerIO m) => [RequestHandler m]
defaultRequestHandlers = [RequestHandler methodToolsList listToolsRequestHandler, RequestHandler methodToolsCall callToolRequestHandler]

stdioServer :: (MonadLoggerIO m)
            => [ToolCallHandler m]
            -> [RequestHandler m]
            -> m ()
stdioServer toolCallHandlers requestHandlers = do
  liftIO . SIO.hPrint SIO.stderr $ ("Server starting." :: String)
  liftIO $ SIO.hSetBuffering SIO.stdin SIO.LineBuffering
  liftIO $ SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  liftIO $ SIO.hSetBuffering SIO.stderr SIO.LineBuffering
  server stdin stdout toolCallHandlers requestHandlers
  liftIO . SIO.hPrint SIO.stderr $ ("Server exiting." :: String)

listToolsRequestHandler   :: (MonadLoggerIO m) => ListToolsRequest -> MCPT m (Maybe (Either MCPError ListToolsResult))
listToolsRequestHandler _ = do
  ctx <- ask
  ts <- fmap serverTools . liftIO . atomically . readTVar $ ctx

  return . Just . Right $ ListToolsResult (tool <$> ts)

-- TODO: response content types other than just text
callToolRequestHandler     :: (MonadLoggerIO m) => CallToolRequest -> MCPT m (Maybe (Either MCPError CallToolResult))
callToolRequestHandler req = do
  ctx <- ask
  ts <- fmap serverTools . liftIO . atomically . readTVar $ ctx

  result <- maybe (return . Left $ MCPError (-32602) ("Unknown tool: " `append` (getField @"name" req)) Nothing) (return . Right . ($ req) . handler) $ find ((== getField @"name" req) . getField @"name" . tool) ts
  either (return . Just . Left) (fmap Just) result

server :: (MonadLoggerIO m) => ConduitT () ByteString m ()
                            -> ConduitT ByteString Void m ()
                            -> [ToolCallHandler m]
                            -> [RequestHandler m]
                            -> m ()
server input output toolCallHandlers requestHandlers = do
  let input' = readerC $ const input
  let handlersByMethod = H.fromList . fmap (liftA2 (,) requestMethod Prelude.id) $ requestHandlers

  ctx <- liftIO . atomically . newTVar $ initialState toolCallHandlers

  runConduit . runReaderC ctx $ input' .| peekForeverE (mainConduit handlersByMethod)

  where
    mainConduit handlersByMethod = lineAsciiC (mapMC decodeRequest) .| mapMC (liftA2 (>>) (either (logServerError . pack . show) logRequest) (handleRequest handlersByMethod))
                                                                    .| mapWhileC Prelude.id
                                                                    .| mapMC (liftA2 (>>) (either (const $ return ()) logResponse) encodeResponse)
                                                                    .| readerC (const output)

    decodeRequest :: (MonadLoggerIO m) => ByteString -> MCPT m (Either MCPError JSONRPCRequest)
    decodeRequest = either (return . Left . (flip (MCPError (-32600)) Nothing) . ("invalid JSON-RPC 2.0 request: " `append`) . pack)
                           (return . Right) . eitherDecodeStrict . C.strip

    handleRequest                      :: (MonadLoggerIO m) => H.HashMap Text (RequestHandler m) -> Either MCPError JSONRPCRequest -> MCPT m (Maybe (Either MCPError Value))
    handleRequest handlersByMethod req = do
      ctx <- ask
      st  <- fmap currentState . liftIO . atomically . readTVar $ ctx

      case req of
        (Left e)  -> return . Just $ Left e
        (Right q) -> case st of
          ServerStart        -> handleWith initializeRequestHandler q
          ServerInitializing -> handleWith initializedNotificationHandler q
          ServerOperational  -> serverMainHandler handlersByMethod q

    serverMainHandler                    :: (MonadLoggerIO m) => H.HashMap Text (RequestHandler m) -> JSONRPCRequest -> MCPT m (Maybe (Either MCPError Value))
    serverMainHandler handlersByMethod q = maybe (return . Just . Left $ MCPError (-32601) ("Method not found: " `append` (method q)) Nothing)
                                                 (\(RequestHandler m h) -> handleWith h q) $ H.lookup (method q) handlersByMethod

    handleWith     :: (GToJSON' Value Zero (Rep r), MCPRequest q, MCPResult r, MonadLoggerIO m)
                   => (q -> MCPT m (Maybe (Either MCPError r)))
                   -> (JSONRPCRequest -> MCPT m (Maybe (Either MCPError Value)))
    handleWith h q = either (const $ (return . Just . Left $ invalidParams)) handle . parseEither parseJSON . reqParams $ q
      where invalidParams = MCPError (-32602) "invalid parameters for request" Nothing
            handle        = h >=> return . fmap (either Left (Right . mcpResultJSON (id q)))
            reqParams     = maybe (Object M.empty) Prelude.id . params

    initializeRequestHandler     :: (MonadLoggerIO m) => InitializeRequest -> MCPT m (Maybe (Either MCPError InitializeResult))
    initializeRequestHandler req = do
      ctx <- ask
      liftIO . atomically $ do
        serverCaps <- fmap serverCapabilities . readTVar $ ctx

        modifyTVar ctx (\s -> s { clientCapabilities = Just $ getField @"capabilities" req
                                , currentState       = ServerInitializing
                                })

        return . Just . Right $ InitializeResult
          { capabilities = serverCaps
          , serverInfo   = Implementation "haskell-network-mcp" "v0.0.1"
          , instructions = Nothing
          }

    initializedNotificationHandler   :: (MonadLoggerIO m) => InitializedNotification -> MCPT m (Maybe (Either MCPError NotificationResult))
    initializedNotificationHandler _ = do
      ctx <- ask
      logServerDebug "Initialized."
      liftIO . atomically $ modifyTVar ctx (\s -> s { currentState = ServerOperational })
      return Nothing

    logRequest :: (MonadLoggerIO m) => JSONRPCRequest -> MCPT m ()
    logRequest = logServerDebug . ("=<< Received request: " `append`) . toStrict . encodeToLazyText

    logResponse = logServerDebug . (">>= Sending response: " `append`) . toStrict . encodeToLazyText

    encodeResponse = maybe (return empty) return . either (return . (flip C.snoc $ '\n') . B.toStrict . encode) (return . (flip C.snoc $ '\n') . B.toStrict . encode)

logServer         :: (MonadLoggerIO m)
                  => LogLevel
                  -> Text
                  -> MCPT m ()
logServer lvl msg = ask >>= fmap currentState . liftIO . atomically . readTVar
                        >>= (flip (flip logWithoutLoc $ lvl) $ msg) . (("Server@" `append`) . pack . show)

logServerError :: (MonadLoggerIO m) => Text -> MCPT m ()
logServerError = logServer LevelError

logServerDebug :: (MonadLoggerIO m) => Text -> MCPT m ()
logServerDebug = logServer LevelDebug

logServerWarn  :: (MonadLoggerIO m) => Text -> MCPT m ()
logServerWarn  = logServer LevelWarn

logServerInfo  :: (MonadLoggerIO m) => Text -> MCPT m ()
logServerInfo  = logServer LevelInfo

handleToolCall     :: (FromJSON r, MonadIO m) => (r -> MCPT m (Either ToolError CallToolResult)) -> (CallToolRequest -> MCPT m (Either ToolError CallToolResult))
handleToolCall h r = either (return . Left . ArgumentError . pack) h . parseEither parseJSON
                                                                     $ maybe (Object M.empty) Prelude.id (fmap Object (arguments r))
