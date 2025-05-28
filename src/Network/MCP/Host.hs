{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.MCP.Host where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
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
import Data.Conduit.Combinators hiding(print)
import Data.Conduit.Network
import Data.Text
import Data.Text.Lazy hiding(append, pack, Text)
import Data.Vector

import Network.JSONRPC
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

initialContext :: ClientContext
initialContext = ClientContext ClientStart

client     :: (MonadLoggerIO m, MonadUnliftIO m)
           => CreateProcess
           -> m ()
           -> m ()
client p m = do
  errHdl <- liftIO $ openFile "/dev/null" WriteMode
  (min, mout, merr, h) <- liftIO $ createProcess p { std_in  = CreatePipe
                                                   , std_out = CreatePipe
                                                   , std_err = UseHandle errHdl
                                                   }
  let handles = min >>= (\input -> mout >>= (\out -> return (input, out)))

  maybe (return ()) (\(input, out) -> do
    liftIO $ hSetBuffering input LineBuffering
    liftIO $ hSetBuffering out LineBuffering

    runJSONRPCT V2 False (sinkHandle input) (sourceHandle out) $ do
      lift . logWithoutLoc "Client" LevelDebug $ ("Initializing." :: Text)

      res <- sendRequest $ InitializeRequest
        { capabilities = dummyClientCaps
        , clientInfo   = clientImplementation
        }

      lift . logWithoutLoc "Client" LevelDebug $ ("Sent InitializeRequest." :: Text)

      lift . logWithoutLoc "Client" LevelDebug . ("Received response: " `append`) . toStrict . encodeToLazyText $ (res :: Maybe (Either ErrorObj InitializeResult))

      res2 <- sendRequest $ InitializedNotification

      lift . logWithoutLoc "Client" LevelDebug . toStrict . encodeToLazyText $ (res2 :: Maybe (Either ErrorObj InitializeResult))

      return ()) $ handles
