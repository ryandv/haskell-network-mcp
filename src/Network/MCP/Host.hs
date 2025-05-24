{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Host where

import Control.Concurrent.STM.TVar

import Control.Applicative
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Logger

import GHC.Records

import Data.Aeson
import Data.Aeson.Text
import Data.Conduit.Combinators hiding(print)
import Data.Conduit.Network
import Data.Text
import Data.Text.Lazy hiding(append, pack, Text)
import Data.Vector

import Network.JSONRPC
import Network.MCP.Types

clientImplementation :: Implementation
clientImplementation = Implementation "haskell-network-mcp" "v0.0.1"

dummyClientCaps :: ClientCapabilities
dummyClientCaps = ClientCapabilities
  { roots    = Nothing
  , sampling = Nothing
  }

client :: LoggingT IO ()
client = jsonrpcTCPClient V2 True (clientSettings 6666 "localhost") $ do
  lift . logWithoutLoc "Client" LevelDebug $ ("Initializing." :: Text)
  res <- sendRequest $ InitializeRequest
    { capabilities = dummyClientCaps
    , clientInfo   = clientImplementation
    }
  lift . logWithoutLoc "Client" LevelDebug . toStrict . encodeToLazyText $ (res :: Maybe (Either ErrorObj InitializeResult))

  res2 <- sendRequest $ InitializedNotification
  lift . logWithoutLoc "Client" LevelDebug . toStrict . encodeToLazyText $ (res2 :: Maybe (Either ErrorObj InitializeResult))
  return ()

initializeResultBuilder     :: InitializeRequest -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) (Either ErrorObj InitializeResult)
initializeResultBuilder req = do
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

listToolsResultBuilder     :: ListToolsRequest -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) (Either ErrorObj ListToolsResult)
listToolsResultBuilder req = do
  ctx <- ask
  ts <- fmap serverTools . liftIO . atomically . readTVar $ ctx

  return . Right $ ListToolsResult ts

data ServerState = ServerStart | ServerInitializing | ServerInitialized deriving(Eq, Show)
data ServerContext = ServerContext
  { currentState       :: ServerState
  , clientCapabilities :: Maybe ClientCapabilities
  , serverCapabilities :: ServerCapabilities
  , serverTools        :: Vector Tool
  } deriving(Eq, Show)

initialState         :: ServerCapabilities -> Vector Tool -> ServerContext
initialState caps ts = ServerContext ServerStart Nothing caps ts

server                 :: ServerCapabilities -> Vector Tool -> (Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()) -> LoggingT IO ()
server caps ts handler = do
  ctx <- liftIO . atomically . newTVar $ initialState caps ts

  runJSONRPCT V2 False stdout stdin . (flip runReaderT) ctx $ (forever serverLoop) <|> return ()

  where
    serverLoop :: ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    serverLoop = lift receiveRequest >>= maybe (logNothingRequest >> shutdown)
                                               (liftA2 (>>) logRequest handleRequest)

    handleRequest                     :: Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    handleRequest req                 = do
      ctx <- ask
      state <- fmap currentState . liftIO . atomically . readTVar $ ctx
      case state of
        ServerStart        -> handleInitializeRequest req
        ServerInitializing -> handleInitializedNotification req
        ServerInitialized  -> case getReqMethod req of
          methodToolsList -> handleListToolsRequest req

    handleInitializeRequest           :: Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    handleInitializeRequest req       = do
      ctx <- ask
      res <- lift . buildResponse (flip runReaderT ctx . initializeResultBuilder) $ req
      maybe (return ()) sendLoggedResponse $ res

    handleInitializedNotification     :: Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    handleInitializedNotification req = do
      ctx <- ask
      logServerDebug "Initialized."
      liftIO . atomically $ modifyTVar ctx (\s -> s { currentState = ServerInitialized })

    handleListToolsRequest            :: Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    handleListToolsRequest req        = do
      ctx <- ask
      res <- lift . buildResponse (flip runReaderT ctx . listToolsResultBuilder) $ req
      maybe (return ()) sendLoggedResponse $ res

    sendLoggedResponse                = liftA2 (>>) (lift . sendResponse) logResponse

    logNothingRequest                 = logServerWarn "Stream empty."
    logRequest                        = logServerDebug . ("Received request: " `append`) . toStrict . encodeToLazyText

    logResponse                       :: Response -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    logResponse                       = logServerDebug . ("Handled request: " `append`) . toStrict . encodeToLazyText

    logServer                         :: LogLevel -> Text -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    logServer lvl msg                 = do
      ctx <- ask
      state <- fmap currentState . liftIO . atomically $ readTVar ctx
      logWithoutLoc (("Server@" `append`) . pack . show $ state) lvl msg
    logServerError = logServer LevelError
    logServerDebug = logServer LevelDebug
    logServerWarn  = logServer LevelWarn

    shutdown = fail "Shutting down."
