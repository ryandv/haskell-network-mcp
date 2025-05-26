{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Server where

import qualified Prelude
import Prelude hiding(lookup, null)

import Control.Concurrent.STM.TVar

import Control.Applicative
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger

import GHC.Records

import Data.Aeson
import Data.Aeson.Text
import Data.ByteString(ByteString)
import Data.Conduit
import Data.Conduit.Combinators hiding(null, print)
import Data.Conduit.Network
import Data.HashMap.Strict hiding(null)
import Data.Text hiding(null)
import Data.Text.Lazy hiding(append, null, pack, Text)
import Data.Vector hiding(fromList)

import Network.JSONRPC
import Network.MCP.Types

data ToolCallHandler    = forall q r m. (FromRequest q, FromResponse r, MonadIO m) => ToolCallHandler (String, q -> m (Either ErrorObj r))
type ToolCallHandlers   = [ToolCallHandler]

data ServerState = ServerStart | ServerInitializing | ServerOperational deriving(Eq, Show)
data ServerContext = ServerContext
  { currentState       :: ServerState
  , clientCapabilities :: Maybe ClientCapabilities
  , serverCapabilities :: ServerCapabilities
  , serverTools        :: Vector Tool
  }

type MCPT m = ReaderT (TVar ServerContext) (JSONRPCT m)

initialState                :: Vector Tool -> ServerContext
initialState ts | null ts   = ServerContext ServerStart Nothing noCapabilities ts
                | otherwise = ServerContext ServerStart Nothing (noCapabilities { tools = Just $ ListChangedCapability False }) ts

server :: (Alternative m, MonadFail m, MonadLoggerIO m, MonadUnliftIO m)
       => ConduitT ByteString Void m ()
       -> ConduitT () ByteString m ()
       -> Vector Tool
       -> m ()
server out input ts = do
  ctx <- liftIO . atomically . newTVar $ initialState ts

  runJSONRPCT V2 False out input . (flip runReaderT) ctx $ (forever serverLoop) <|> return ()

  where
    serverLoop :: (Alternative m, MonadFail m, MonadLoggerIO m) => MCPT m ()
    serverLoop = lift receiveRequest >>= maybe (logEndOfStream >> shutdown)
                                               (liftA2 (>>) logRequest handleRequest)

    handleRequest                     :: (MonadLoggerIO m) => Request -> MCPT m ()
    handleRequest req                 = do
      ctx      <- ask
      state    <- fmap currentState   . liftIO . atomically . readTVar $ ctx

      case state of
        ServerStart        -> handleWith initializeResultHandler req
        ServerInitializing -> handleInitializedNotification req
        ServerOperational  -> case getReqMethod req of
                                methodToolsList -> handleWith listToolsResultHandler req

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

      return . Right $ ListToolsResult ts

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
