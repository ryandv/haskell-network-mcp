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

  , handleToolCall
  , newServer
  , newStdioServer
  , server
  , stdioServer
  , toolBuilder
  ) where

import qualified Prelude
import Prelude hiding(foldr, id, lookup)

import Conduit

import Control.Concurrent.STM.TVar

import Control.Applicative hiding(empty)
import Control.Monad
import Control.Monad.Reader(MonadReader)
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger

import GHC.Generics
import GHC.Records

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Aeson.Key
import Data.ByteString(ByteString, empty)
import Data.Conduit
import Data.Conduit.Combinators hiding(find, null, print)
import Data.Conduit.Network
import Data.HashMap.Strict hiding(empty, foldr, fromList, null)
import Data.Maybe
import Data.Text hiding(empty, find, foldr, null)
import Data.Text.Lazy hiding(append, empty, find, foldr, null, pack, Text, unpack)
import Data.Vector hiding(empty, null)
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import qualified Data.UUID as UUID

import Network.JSONRPC
import Network.MCP.Types

import qualified System.IO as SIO

data ToolCallHandler m  = ToolCallHandler
  { tool    :: Tool
  , handler :: CallToolRequest -> MCPT m (Either ErrorObj CallToolResult)
  }

data NewToolCallHandler m  = NewToolCallHandler
  { newTool    :: Tool
  , newHandler :: CallToolRequest -> ReaderT (TVar (NewServerContext m)) m (Either ErrorObj CallToolResult)
  }

data ServerState = ServerStart | ServerInitializing | ServerOperational deriving(Eq, Show)
data ServerContext m = ServerContext
  { currentState           :: ServerState
  , clientCapabilities     :: Maybe ClientCapabilities
  , serverCapabilities     :: ServerCapabilities
  , serverTools            :: Vector (ToolCallHandler m)
  }

data NewServerContext m = NewServerContext
  { newCurrentState           :: ServerState
  , newClientCapabilities     :: Maybe ClientCapabilities
  , newServerCapabilities     :: ServerCapabilities
  , newServerTools            :: Vector (NewToolCallHandler m)
  }

-- TODO: tool annotations
data ToolArgumentDescriptor = ToolArgumentDescriptor
  { argName        :: Text
  , argDescription :: Text
  , argType        :: Text
  , argRequired    :: Bool
  } deriving(Eq, Show)

data ToolError = ArgumentError Text | ExecutionError Text

type MCPT m = ReaderT (TVar (ServerContext m)) (JSONRPCT m)

-- TODO: tool annotations
newToolBuilder            :: (Monad m)
                          => Text
                          -> Text
                          -> [ToolArgumentDescriptor]
                          -> (CallToolRequest -> ReaderT (TVar (NewServerContext m)) m (Either ToolError CallToolResult))
                          -> NewToolCallHandler m
newToolBuilder n d args h = NewToolCallHandler tl (h >=> either (return . mapError) (return . Right))
  where tl   = Tool n (Just d) is Nothing
        is   = InputSchema (Just . M.fromList
                                 . fmap (\a -> (fromText (argName a), object [ ("type" .= argType a), ("description" .= argDescription a) ]))
                                 $ args)
                           (Just $ fromList reqd)
        reqd = argName <$> Prelude.filter argRequired args
        mapError (ExecutionError e) = Right . flip CallToolResult (Just True) . fromList . return . flip TextContent Nothing $ e
        mapError (ArgumentError e ) = Left $ errorParams Null ("Missing argument: " `mappend` (unpack e))


initialState                :: [ToolCallHandler m] -> ServerContext m
initialState ts | null ts   = ServerContext ServerStart Nothing noCapabilities (fromList ts)
                | otherwise = ServerContext ServerStart Nothing (noCapabilities { tools = Just $ ListChangedCapability False }) (fromList ts)

newInitialState                :: [NewToolCallHandler m] -> NewServerContext m
newInitialState ts | null ts   = NewServerContext ServerStart Nothing noCapabilities (fromList ts)
                   | otherwise = NewServerContext ServerStart Nothing (noCapabilities { tools = Just $ ListChangedCapability False }) (fromList ts)

newLogServer         :: (MonadLoggerIO m)
                     => LogLevel
                     -> Text
                     -> ReaderT (TVar (NewServerContext m)) m ()
newLogServer lvl msg = ask >>= fmap newCurrentState . liftIO . atomically . readTVar
                           >>= (flip (flip logWithoutLoc $ lvl) $ msg) . (("Server@" `append`) . pack . show)

newLogServerError :: (MonadLoggerIO m) => Text -> ReaderT (TVar (NewServerContext m)) m ()
newLogServerError = newLogServer LevelError

newLogServerDebug :: (MonadLoggerIO m) => Text -> ReaderT (TVar (NewServerContext m)) m ()
newLogServerDebug = newLogServer LevelDebug

newLogServerWarn  :: (MonadLoggerIO m) => Text -> ReaderT (TVar (NewServerContext m)) m ()
newLogServerWarn  = newLogServer LevelWarn

newLogServerInfo  :: (MonadLoggerIO m) => Text -> ReaderT (TVar (NewServerContext m)) m ()
newLogServerInfo  = newLogServer LevelInfo

newStdioServer :: (MonadLoggerIO m)
               => [NewToolCallHandler m]
               -> m ()
newStdioServer ts = do
  liftIO . SIO.hPrint SIO.stderr $ "Server starting."
  liftIO $ SIO.hSetBuffering SIO.stdin SIO.LineBuffering
  liftIO $ SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  liftIO $ SIO.hSetBuffering SIO.stderr SIO.LineBuffering
  newServer stdin stdout ts
  liftIO . SIO.hPrint SIO.stderr $ "Server exiting."

newServer :: (MonadLoggerIO m)
          => ConduitT () ByteString m ()
          -> ConduitT ByteString Void m ()
          -> [NewToolCallHandler m]
          -> m ()
newServer input output ts = do
  let input' = readerC $ const input

  ctx <- liftIO . atomically . newTVar $ newInitialState ts

  runConduit . runReaderC ctx $ input' .| peekForeverE (lineAsciiC (mapWhile (decodeRequest . C.strip))
                                       .| mapMC (liftA2 (>>) logRequest handleRequest)
                                       .| mapWhileC Prelude.id
                                       .| mapMC (liftA2 (>>) logResponse encodeResponse)
                                       .| readerC (const output))
  where
    decodeRequest :: ByteString -> Maybe JSONRPCRequest
    decodeRequest = decodeStrict

    handleRequest     :: (MonadLoggerIO m) => JSONRPCRequest -> ReaderT (TVar (NewServerContext m)) m (Maybe (Either MCPError Value))
    handleRequest req = do
      ctx <- ask
      state <- fmap newCurrentState . liftIO . atomically . readTVar $ ctx

      case state of
        ServerStart        -> handleWith initializeRequestHandler req
        ServerInitializing -> handleWith initializedNotificationHandler req
        ServerOperational  -> return . Just . Right $ object []

    handleWith     :: (GToJSON' Value Zero (Rep r), MCPRequest q, MCPResult r, MonadLoggerIO m)
                   => (q -> ReaderT (TVar (NewServerContext m)) m (Maybe (Either MCPError r)))
                   -> (JSONRPCRequest -> ReaderT (TVar (NewServerContext m)) m (Maybe (Either MCPError Value)))
    handleWith h q = either (const $ (return . Just . Left $ invalidParams)) (handle h) . parseEither parseJSON . maybe (Object M.empty) Prelude.id . params $ q
      where invalidParams = MCPError (-32602) "invalid parameters for request" Nothing
            handle h      = h >=> return . fmap (either Left (Right . mcpResultJSON (id q)))

    initializeRequestHandler     :: (MonadLoggerIO m) => InitializeRequest -> ReaderT (TVar (NewServerContext m)) m (Maybe (Either MCPError InitializeResult))
    initializeRequestHandler req = do
      ctx <- ask
      liftIO . atomically $ do
        serverCaps <- fmap newServerCapabilities . readTVar $ ctx

        modifyTVar ctx (\s -> s { newClientCapabilities = Just $ getField @"capabilities" req
                                , newCurrentState       = ServerInitializing
                                })

        return . Just . Right $ InitializeResult
          { capabilities = serverCaps
          , serverInfo   = Implementation "haskell-network-mcp" "v0.0.1"
          , instructions = Nothing
          }

    initializedNotificationHandler     :: (MonadLoggerIO m) => InitializedNotification -> ReaderT (TVar (NewServerContext m)) m (Maybe (Either MCPError NotificationResult))
    initializedNotificationHandler req = do
      ctx <- ask
      newLogServerDebug "Initialized."
      liftIO . atomically $ modifyTVar ctx (\s -> s { newCurrentState = ServerOperational })
      return Nothing

    logRequest :: (MonadLoggerIO m) => JSONRPCRequest -> ReaderT (TVar (NewServerContext m)) m ()
    logRequest = newLogServerDebug . ("=<< Received request: " `append`) . toStrict . encodeToLazyText

    logResponse = newLogServerDebug . (">>= Sending response: " `append`) . toStrict . encodeToLazyText

    encodeResponse = maybe (return empty) return . either (return . (flip C.snoc $ '\n') . B.toStrict . encode) (return . (flip C.snoc $ '\n') . B.toStrict . encode)
    --encodeResponse = maybe (return B.empty) return . either (return . encode) (return . encode . mcpResultJSON (fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440000"))

--------------------------------------------------------------------------------
-- old
--------------------------------------------------------------------------------
--
-- TODO: tool annotations
toolBuilder :: (Monad m)
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
        mapError (ArgumentError e ) = Left $ errorParams Null ("Missing argument: " `mappend` (unpack e))

handleToolCall     :: (FromJSON r, MonadIO m, MonadLogger m) => (r -> MCPT m (Either ToolError CallToolResult)) -> (CallToolRequest -> MCPT m (Either ToolError CallToolResult))
handleToolCall h r = either (return . Left . ArgumentError . pack) h . parseEither parseJSON
                                                                     $ maybe (Object M.empty) Prelude.id (fmap Object (arguments r))

stdioServer :: (Alternative m, MonadFail m, MonadLoggerIO m, MonadUnliftIO m)
            => [(Request -> MCPT m ())]
            -> [ToolCallHandler m]
            -> m ()
stdioServer hs ts = do
  liftIO $ SIO.hSetBuffering SIO.stdin SIO.LineBuffering
  liftIO $ SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  server stdin stdout hs ts

server :: (Alternative m, MonadFail m, MonadLoggerIO m, MonadUnliftIO m)
       => ConduitT () ByteString m ()
       -> ConduitT ByteString Void m ()
       -> [(Request -> MCPT m ())]
       -> [ToolCallHandler m]
       -> m ()
server input out customHandlers ts = do
  ctx <- liftIO . atomically . newTVar $ initialState ts

  runJSONRPCT V2 False out input . (flip runReaderT) ctx
                                 $ (forever $ serverLoop (fromList customHandlers)) <|> return ()

  where
    serverLoop                :: (Alternative m, MonadFail m, MonadLoggerIO m, MonadUnliftIO m) => Vector (Request -> MCPT m ()) -> MCPT m ()
    serverLoop customHandlers = lift receiveRequest >>= maybe (logEndOfStream >> shutdown)
                                                              (liftA2 (>>) logRequest (handleRequest customHandlers))

    handleRequest                     :: (Alternative m, MonadFail m, MonadLoggerIO m, MonadUnliftIO m) => Vector (Request -> MCPT m ()) -> Request -> MCPT m ()
    handleRequest customHandlers req  = do
      ctx      <- ask
      state    <- fmap currentState   . liftIO . atomically . readTVar $ ctx

      case state of
        ServerStart        -> handleWith initializeResultHandler req
        ServerInitializing -> handleInitializedNotification req
        ServerOperational  -> serverMainHandler customHandlers req

    -- TODO: proper seed value for foldr; append method missing handler to customHandlers
    serverMainHandler     :: (Alternative m, MonadFail m, MonadLoggerIO m, MonadUnliftIO m)
                          => Vector (Request -> MCPT m ())
                          -> Request
                          -> MCPT m ()
    serverMainHandler customHandlers req | getReqMethod req == methodToolsList = handleWith listToolsResultHandler req
                                         | getReqMethod req == methodToolsCall = handleWith callToolResultHandler req
                                         |                           otherwise = foldr ((<|>) . ($ req)) (fail ("Handler not found for method " `mappend` (unpack $ getReqMethod req))) customHandlers

    handleWith             :: (FromRequest q, MonadLoggerIO m, ToJSON r) => (q -> MCPT m (Either ErrorObj r)) -> Request -> MCPT m ()
    handleWith handler req = do
      ctx <- ask
      res <- lift . buildResponse (flip runReaderT ctx . handler) $ req
      maybe (return ()) respond $ res

    handleInitializedNotification     :: (MonadLoggerIO m) => Request -> MCPT m ()
    handleInitializedNotification req = do
      ctx <- ask
      logServerDebug "Initialized."
      liftIO . atomically $ modifyTVar ctx (\s -> s { currentState = ServerOperational })

    initializeResultHandler     :: (MonadLoggerIO m) => InitializeRequest -> MCPT m (Either ErrorObj InitializeResult)
    initializeResultHandler req = do
      ctx <- ask
      liftIO . atomically $ do
        serverCaps <- fmap serverCapabilities . readTVar $ ctx

        modifyTVar ctx (\s -> s { clientCapabilities = Just $ getField @"capabilities" req
                                , currentState       = ServerInitializing
                                })

        return . Right $ InitializeResult
          { capabilities = serverCaps
          , serverInfo   = Implementation "haskell-network-mcp" "v0.0.1"
          , instructions = Nothing
          }

    listToolsResultHandler     :: (MonadLoggerIO m) => ListToolsRequest -> MCPT m (Either ErrorObj ListToolsResult)
    listToolsResultHandler req = do
      ctx <- ask
      ts <- fmap serverTools . liftIO . atomically . readTVar $ ctx

      return . Right $ ListToolsResult (tool <$> ts)

    -- TODO: response content types other than just text
    callToolResultHandler     :: (MonadLoggerIO m) => CallToolRequest -> MCPT m (Either ErrorObj CallToolResult)
    callToolResultHandler req = do
      ctx <- ask
      ts <- fmap serverTools . liftIO . atomically . readTVar $ ctx

      result <- maybe (return . Left $ errorParams Null ("Unknown tool: " `mappend` (unpack $ getField @"name" req))) (return . Right . ($ req) . handler) $ find ((== getField @"name" req) . getField @"name" . tool) ts
      either (return . Left) Prelude.id result

    methodMissingHandler   :: (MonadLoggerIO m) => Method -> Value -> MCPT m (Either ErrorObj Value)
    methodMissingHandler m = (const $ return . Left . errorMethod $ m)

    logEndOfStream                    :: (Alternative m, MonadFail m, MonadLoggerIO m) => MCPT m ()
    logEndOfStream                    = logServerWarn "Stream empty."

    logRequest                        :: (Alternative m, MonadFail m, MonadLoggerIO m) => Request -> MCPT m ()
    logRequest                        = logServerDebug . ("Received request: " `append`) . toStrict . encodeToLazyText

    shutdown                          :: (Alternative m, MonadFail m, MonadLoggerIO m) => MCPT m ()
    shutdown                          = fail "Shutting down."

respond :: (MonadLoggerIO m) => Response -> MCPT m ()
respond = liftA2 (>>) (lift . sendResponse) logResponse
  where logResponse = logServerDebug . ("Handled request: " `append`) . toStrict . encodeToLazyText

logServer                         :: (MonadLoggerIO m) => LogLevel -> Text -> MCPT m ()
logServer lvl msg                 = do
  ctx <- ask
  state <- fmap currentState . liftIO . atomically $ readTVar ctx
  logWithoutLoc (("Server@" `append`) . pack . show $ state) lvl msg

logServerError :: (MonadLoggerIO m) => Text -> MCPT m ()
logServerError = logServer LevelError

logServerDebug :: (MonadLoggerIO m) => Text -> MCPT m ()
logServerDebug = logServer LevelDebug

logServerWarn  :: (MonadLoggerIO m) => Text -> MCPT m ()
logServerWarn  = logServer LevelWarn
