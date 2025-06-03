{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Host where

import Conduit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
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
import Data.ByteString hiding(append, getLine, pack, toStrict)
import Data.Conduit.Combinators hiding(print)
import Data.Text hiding(empty)
import Data.Text.Lazy hiding(append, empty, pack, Text)
import Data.UUID
import Data.UUID.V4
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
  -- prologue
  ctx      <- liftIO . atomically . newTVar $ ClientContext ClientStart
  q        <- liftIO . atomically $ newTQueue

  initUUID <- liftIO $ nextRandom

  liftIO $ hSetBuffering input LineBuffering
  liftIO $ hSetBuffering output LineBuffering

  logWithoutLoc "Client" LevelDebug $ ("Initializing." :: Text)

  -- user input consumer
  liftIO . forkIO . forever $ do
    reqs <- liftIO . atomically . flushTQueue $ q
    runConduit $ yieldMany reqs .| sinkHandle input

  -- user input producer
  liftIO . forkIO . forever $ do
    ln <- getLine
    liftIO . atomically . writeTQueue q . C.pack $ ln

  -- mcp init
  let initreq = InitializeRequest dummyClientCaps clientImplementation
  let initReqJSON = (flip C.snoc $ '\n') . L.toStrict . encode $ mcpRequestJSON (Just . Right $ toText initUUID) initreq

  liftIO . atomically . writeTQueue q $ initReqJSON
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
      rid <- liftIO $ nextRandom

      case res of
        (Left e)  -> (liftIO . print $ "Failed to decode JSON response: " `append` e) >> return Nothing
        (Right r) -> liftIO . atomically $ do
          c <- readTVar $ ctx

          case currentState c of
            ClientStart       -> (swapTVar ctx $ c { currentState = ClientOperational }) >> (return $ handleWith initializeResultHandler rid r)
            ClientOperational -> return Nothing

    handleWith       :: (GToJSON' Value Zero (Rep q), MCPRequest q, MCPResult r) => (r -> Maybe (Either Text q)) -> UUID -> (Value -> Maybe (Either Text Value))
    handleWith h rid = either (Just . Left . ("Failed to decode MCP response: " `append`) . pack)
                              (fmap (either Left (Right . mcpRequestJSON (Just . Right $ toText rid))) . h) . parseEither (withObject "MCP Response" (.: "result"))
                            --(h >=> fmap (either Left (Right . mcpRequestJSON (Just . Right $ toText rid))))

    initializeResultHandler     :: InitializeResult -> Maybe (Either Text InitializedNotification)
    initializeResultHandler res = Just . Right $ InitializedNotification

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
