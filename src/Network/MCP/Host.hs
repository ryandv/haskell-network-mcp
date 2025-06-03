{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Host where

import Conduit

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.ByteString hiding(append, pack, toStrict)
import Data.Conduit.Combinators hiding(print)
import Data.Text hiding(empty)
import Data.Text.Lazy hiding(append, empty, pack, Text)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import GHC.Generics

import Network.MCP.Server
import Network.MCP.Types

import System.IO
import System.Process

clientImplementation :: Implementation
clientImplementation = Implementation "haskell-network-mcp" "v0.0.1"

dummyClientCaps :: ClientCapabilities
dummyClientCaps = ClientCapabilities
  { roots    = Nothing
  , sampling = Nothing
  }

data ClientState = ClientStart | ClientInitializing | ClientOperational deriving(Eq, Show)
data ClientContext = ClientContext
  { currentState :: ClientState
  }

type MCPClientT m = ReaderT (TVar ClientContext) m

initialContext :: ClientContext
initialContext = ClientContext ClientStart

clientWith input output = do
  ctx <- liftIO . atomically . newTVar $ ClientContext ClientStart

  liftIO $ hSetBuffering input LineBuffering
  liftIO $ hSetBuffering output LineBuffering

  logWithoutLoc "Client" LevelDebug $ ("Initializing." :: Text)

  let initreq = InitializeRequest { capabilities = dummyClientCaps
                                  , clientInfo   = clientImplementation
                                  }
  let initReqJSON = (flip C.snoc $ '\n') . L.toStrict . encode $ mcpRequestJSON (Just $ Left 0) initreq
  let initNotJSON = (flip C.snoc $ '\n') . L.toStrict . encode $ mcpRequestJSON Nothing InitializedNotification
  let initJSONs   = [initReqJSON, initNotJSON]

  liftIO $ print initJSONs

  runConduit $ yieldMany initJSONs .| sinkHandle input
  logWithoutLoc "Client" LevelDebug $ ("Sent InitializeRequest." :: Text)

  runConduit . runReaderC ctx $ sourceHandle output .| peekForeverE (mainConduit input output)
  where
    mainConduit input output = lineAsciiC (mapMC decodeResponse) .| mapMC (liftA2 (>>) (either (const $ return ()) logResponse) handleResponse)
                                                                 .| mapWhileC Prelude.id
                                                                 .| mapMC (liftA2 (>>) (either (const $ return ()) logRequest) encodeRequest)
                                                                 .| readerC (const $ sinkHandle input)

    decodeResponse :: (MonadLoggerIO m) => ByteString -> MCPClientT m (Either Text Value)
    decodeResponse = either (return . Left . pack) (return . Right) . eitherDecodeStrict . C.strip

    handleResponse     :: (MonadLoggerIO m) => Either Text Value -> MCPClientT m (Maybe (Either Text Value))
    handleResponse res = do
      ctx <- ask
      st  <- fmap currentState . liftIO . atomically . readTVar $ ctx

      case res of
        (Left e)  -> (liftIO . print $ "Failed to decode JSON response: " `append` e) >> return Nothing
        (Right r) -> case st of
          ClientStart       -> handleWith initializeResultHandler r
          ClientOperational -> undefined

    handleWith   :: (GToJSON' Value Zero (Rep q), MCPRequest q, MCPResult r, MonadLoggerIO m) => (r -> MCPClientT m (Maybe (Either Text q))) -> (Value -> MCPClientT m (Maybe (Either Text Value)))
    handleWith h = either (return . Just . Left . ("Failed to decode MCP response: " `append`) . pack) (h >=> return . fmap (either Left (Right . mcpRequestJSON (Just $ Left (-1))))) . parseEither (withObject "MCP Response" (.: "result"))

    initializeResultHandler     :: (MonadLoggerIO m) => InitializeResult -> MCPClientT m (Maybe (Either Text InitializedNotification))
    initializeResultHandler res = return . Just . Right $ InitializedNotification

    logRequest    :: (MonadLoggerIO m, ToJSON q) => q -> MCPClientT m ()
    logRequest    = lift . logWithoutLoc "Client" LevelDebug . ("Sending request: " `append`) . toStrict . encodeToLazyText

    logResponse   = lift . logWithoutLoc "Client" LevelDebug . ("Received response: " `append`) . toStrict . encodeToLazyText

    encodeRequest :: (MonadLoggerIO m, ToJSON q) => Either Text q -> MCPClientT m ByteString
    encodeRequest = either (liftIO . print . ("error: " `append`) >=> (const $ return empty)) (return . (flip C.snoc $ '\n') . L.toStrict . encode)

client     :: (MonadLoggerIO m, MonadUnliftIO m)
           => CreateProcess
           -> MCPClientT m ()
           -> m ()
client p m = do
  errHdl <- liftIO $ openFile "/tmp/wat.log" WriteMode
  (min, mout, merr, h) <- liftIO $ createProcess p { std_in  = CreatePipe
                                                   , std_out = CreatePipe
                                                   , std_err = UseHandle errHdl
                                                   }
  let handles = min >>= (\input -> mout >>= (\out -> return (input, out)))
  maybe (return ()) (liftIO . (flip hSetBuffering $ LineBuffering)) merr

  maybe (return ()) (uncurry clientWith) handles
