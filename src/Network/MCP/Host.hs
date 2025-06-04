{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Host
  ( clientWith
  , client
  ) where

import Conduit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Logger

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.ByteString hiding(append, getLine, pack, toStrict)
import Data.Text hiding(empty)
import Data.Text.Lazy hiding(append, empty, pack, Text)
import Data.UUID
import Data.UUID.V4
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import GHC.Generics

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

clientWith :: (MonadLoggerIO m)
           => (ByteString -> IO ByteString)
           -> Handle
           -> Handle
           -> m ()
clientWith userInputHandler input output = do
  -- prologue
  ctx      <- liftIO . atomically . newTVar $ initialContext

  initUUID <- liftIO $ nextRandom

  liftIO $ hSetBuffering input LineBuffering
  liftIO $ hSetBuffering output LineBuffering

  logWithoutLoc "Client" LevelDebug $ ("Initializing." :: Text)

  -- user input thread
  _ <- liftIO . forkIO . forever $ do
    ln           <- getLine
    processedReq <- userInputHandler $ C.pack ln
    runConduit $ yieldMany [processedReq] .| sinkHandle input

  -- mcp init
  let initreq = InitializeRequest dummyClientCaps clientImplementation
  let initReqJSON = (flip C.snoc $ '\n') . L.toStrict . encode $ mcpRequestJSON (Just . Right $ toText initUUID) initreq

  runConduit $ yieldMany [initReqJSON] .| sinkHandle input
  logWithoutLoc "Client" LevelDebug $ ("Sent InitializeRequest." :: Text)

  runConduit . runReaderC ctx $ sourceHandle output .| peekForeverE (mainConduit input)

  where
    mainConduit i = lineAsciiC (mapMC decodeResponse) .| mapMC (liftA2 (>>) (either (const $ return ()) logResponse) handleResponse)
                                                      .| mapWhileC Prelude.id
                                                      .| mapMC (liftA2 (>>) (either (const $ return ()) logRequest) encodeRequest)
                                                      .| readerC (const $ sinkHandle i)

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
            _                 -> return Nothing

    handleWith       :: (GToJSON' Value Zero (Rep q), MCPRequest q, MCPResult r) => (r -> Maybe (Either Text q)) -> UUID -> (Value -> Maybe (Either Text Value))
    handleWith h rid = either mcpResultDecodeError (handle h rid) . parseEither (withObject "MCP Response" (.: "result"))

    mcpResultDecodeError :: String -> Maybe (Either Text Value)
    mcpResultDecodeError = Just . Left . ("Failed to decode MCP response: " `append`) . pack

    handle       :: (GToJSON' Value Zero (Rep q), MCPRequest q) => (r -> Maybe (Either Text q)) -> UUID -> (r -> Maybe (Either Text Value))
    handle h rid = fmap (either Left (Right . mcpRequestJSON (Just . Right $ toText rid))) . h

    initializeResultHandler :: InitializeResult -> Maybe (Either Text InitializedNotification)
    initializeResultHandler = const . Just . Right $ InitializedNotification

    logRequest    :: (MonadLoggerIO m, ToJSON q) => q -> MCPClientT m ()
    logRequest    = lift . logWithoutLoc "Client" LevelDebug . ("Sending request: " `append`) . toStrict . encodeToLazyText

    logResponse   = lift . logWithoutLoc "Client" LevelDebug . ("Received response: " `append`) . toStrict . encodeToLazyText

    encodeRequest :: (MonadLoggerIO m, ToJSON q) => Either Text q -> MCPClientT m ByteString
    encodeRequest = either (liftIO . print . ("error: " `append`) >=> (const $ return empty)) (return . (flip C.snoc $ '\n') . L.toStrict . encode)

client       :: (MonadLoggerIO m)
             => CreateProcess
             -> (ByteString -> IO ByteString)
             -> MCPClientT m ()
             -> m ()
client p f _ = do
  errHdl <- liftIO $ openFile "/tmp/wat.log" WriteMode
  (svin, svout, sverr, _h) <- liftIO $ createProcess p { std_in  = CreatePipe
                                                    , std_out = CreatePipe
                                                    , std_err = UseHandle errHdl
                                                    }
  let handles = svin >>= (\input -> svout >>= (\out -> return (input, out)))
  maybe (return ()) (liftIO . (flip hSetBuffering $ LineBuffering)) sverr

  maybe (return ()) (uncurry (clientWith f)) handles
