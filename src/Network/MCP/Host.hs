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
