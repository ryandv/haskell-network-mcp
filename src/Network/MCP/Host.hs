{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Host where

import Control.Concurrent.STM.TVar

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
import Data.Conduit.Network
import Data.Text
import qualified Data.Text.Lazy as TL

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
  lift . logWithoutLoc "Client" LevelDebug . TL.toStrict . encodeToLazyText $ (res :: Maybe (Either ErrorObj InitializeResponse))
  return ()

initializeResponseBuilder         :: TVar ServerContext -> InitializeRequest -> STM (Either ErrorObj InitializeResponse)
initializeResponseBuilder ctx req = do
  modifyTVar ctx (\s -> s { clientCapabilities = Just $ getField @"capabilities" req
                          , currentState       = ServerInitializing
                          })

  return . Right $ InitializeResponse
    { capabilities = ServerCapabilities
      { logging     = Nothing
      , completions = Nothing
      , prompts     = Nothing
      , resources   = Nothing
      , tools       = Just $ ListChangedCapability False
      }
    , serverInfo = Implementation "haskell-network-mcp" "v0.0.1"
    , instructions = Nothing
    }

data ServerState = ServerStart | ServerInitializing | ServerInitialized deriving(Eq, Show)
data ServerContext = ServerContext
  { currentState       :: ServerState
  , clientCapabilities :: Maybe ClientCapabilities
  } deriving(Eq, Show)

initialState :: ServerContext
initialState = ServerContext ServerStart Nothing

server :: LoggingT IO ()
server = do
  ctx <- liftIO . atomically . newTVar $ initialState

  jsonrpcTCPServer V2 False (serverSettings 6666 "*4") $ runReaderT mainLoop ctx

  where
    mainLoop                          :: ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    mainLoop                          = do
      req <- lift receiveRequest
      maybe logNothingRequest (liftA2 (>>) logRequest handleRequest) $ req

    handleRequest                     :: Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    handleRequest req                 = do
      ctx <- ask
      state <- fmap currentState . liftIO . atomically . readTVar $ ctx
      case state of
        ServerStart        -> handleInitializeRequest req
        ServerInitializing -> handleInitializedNotification req
        ServerInitialized  -> return ()

    handleInitializeRequest           :: Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    handleInitializeRequest req       = do
      ctx <- ask
      res <- lift . buildResponse (liftIO . atomically . initializeResponseBuilder ctx) $ req
      maybe (return ()) sendLoggedResponse $ res

    handleInitializedNotification     :: Request -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    handleInitializedNotification req = do
      ctx <- ask
      liftIO . atomically $ modifyTVar ctx (\s -> s { currentState = ServerInitializing })

    sendLoggedResponse                = liftA2 (>>) (lift . sendResponse) logResponse

    logNothingRequest                 = logServerError "Received request Nothing."
    logRequest                        = logServerDebug . ("Received request: " `append`) . TL.toStrict . encodeToLazyText

    logResponse                       :: Response -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    logResponse                       = logServerDebug . ("Handled request: " `append`) . TL.toStrict . encodeToLazyText

    logServer                         :: LogLevel -> Text -> ReaderT (TVar ServerContext) (JSONRPCT (LoggingT IO)) ()
    logServer lvl msg                 = do
      ctx <- ask
      state <- fmap currentState . liftIO . atomically $ readTVar ctx
      logWithoutLoc (("Server@" `append`) . pack . show $ state) lvl msg
    logServerError = logServer LevelError
    logServerDebug = logServer LevelDebug

main :: IO ()
main = print "hello"
