{-# LANGUAGE OverloadedStrings #-}
module Test.Network.MCP.ServerSpec where

import Conduit

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Aeson
import Data.Aeson.Text
import Data.Maybe
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import Network.MCP.Server
import Network.MCP.Types
import Network.JSONRPC

import Test.Hspec

sourceForever    :: MonadIO m => [B.ByteString] -> ConduitT () B.ByteString m ()
sourceForever bs = do
  yieldMany bs
  liftIO $ threadDelay 1000 -- keep the source open long enough to receive a response

spec :: Spec
spec = describe "MPC server" $ do
  let implementation = Implementation "haskell-network-mcp-test" "v0.0.0.1"

  it "can initialize a connection" $ do
    let clientCaps  = ClientCapabilities Nothing Nothing
    let initreq     = InitializeRequest clientCaps implementation
    let initReqJSON = L.toStrict . encode $ buildRequest V2 initreq (IdInt 1)
    let initNotJSON = L.toStrict . encode $ buildRequest V2 InitializedNotification (IdInt 0)

    let input       = sourceForever [initReqJSON, initNotJSON]
    -- NOTE: this spec can pass vacuously if the sink is empty,
    --       (e.g. the stream is closed before we have a chance to
    --       read anything from it) as no expectations will have
    --       been set in the spec...
    let output      = sinkLazy >>= (return . L.toStrict) -- >>= (liftA2 (>>) (liftIO . print) (return . L.toStrict))
                               >>= liftIO . maybe (expectationFailure "failed to decode InitializeResult") (const $ return ())
                                          . ((>>= fromResponse methodInitialize) :: Maybe Response -> Maybe InitializeResult)
                                          . (decode                              :: L.ByteString -> Maybe Response)
                                          . L.fromStrict

    liftIO . runNoLoggingT $ server output input V.empty

    -- TODO: strengthen expectation in light of above note
    True `shouldBe` True
