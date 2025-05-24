{-# LANGUAGE OverloadedStrings #-}
module Test.Network.MCP.TypesSpec where

import Control.Monad.IO.Class

import Data.Aeson
import Data.Aeson.Text
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Lazy as TL

import Network.MCP.Types
import Network.JSONRPC

import Test.Hspec

-- because aeson's generics are inscrutable in combination with Network.JSONRPC...
spec :: Spec
spec = context "json marshalling" $ do
  let implementation = Implementation "haskell-network-mcp-test" "v0.0.0.1"

  describe "InitializeRequest encoding" $ do
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeRequest clientCaps implementation

    let json       = encode subject
    let jsonText   = TL.unpack $ encodeToLazyText subject
    let req        = buildRequest V2 subject (IdInt 1)
    let rpcWrapped = TL.unpack $ encodeToLazyText req

    it "includes a protocolVersion" $ do
      C.unpack json `shouldContain` "2025-03-26"
      jsonText      `shouldContain` "2025-03-26"
      rpcWrapped    `shouldContain` "2025-03-26"

    it "does not encode capabilities it does not have" $ do
      C.unpack json `shouldNotContain` "roots"
      C.unpack json `shouldNotContain` "sampling"
      jsonText      `shouldNotContain` "roots"
      jsonText      `shouldNotContain` "sampling"
      rpcWrapped    `shouldNotContain` "roots"
      rpcWrapped    `shouldNotContain` "sampling"

  describe "InitializeRequest decoding" $ do
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeRequest clientCaps implementation

    let marshalled = "{\"capabilities\":{},\"clientInfo\":{\"name\":\"haskell-network-mcp-test\",\"version\":\"v0.0.0.1\"},\"protocolVersion\":\"2025-03-26\"}"

    it "decodes" $ do
      decode marshalled `shouldBe` Just subject

  describe "InitializeResponse encoding" $ do
    let serverCaps = ServerCapabilities Nothing Nothing Nothing Nothing Nothing
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeResponse serverCaps implementation Nothing

    let json       = encode subject
    let jsonText   = TL.unpack $ encodeToLazyText subject
    res            <- buildResponse ((const (return . Right $ subject)) :: (Monad m) => InitializeRequest -> m (Either ErrorObj InitializeResponse)) $ buildRequest V2 (InitializeRequest clientCaps implementation) (IdInt 1)
    let rpcWrapped = TL.unpack $ encodeToLazyText $ res

    it "includes a protocolVersion" $ do
      C.unpack json `shouldContain` "2025-03-26"
      jsonText      `shouldContain` "2025-03-26"
      rpcWrapped    `shouldContain` "2025-03-26"

    it "does not encode capabilities nor properties it does not have" $ do
      C.unpack json `shouldNotContain` "logging"
      C.unpack json `shouldNotContain` "completions"
      C.unpack json `shouldNotContain` "prompts"
      C.unpack json `shouldNotContain` "resources"
      C.unpack json `shouldNotContain` "tools"

      C.unpack json `shouldNotContain` "instructions"

      jsonText `shouldNotContain` "logging"
      jsonText `shouldNotContain` "completions"
      jsonText `shouldNotContain` "prompts"
      jsonText `shouldNotContain` "resources"
      jsonText `shouldNotContain` "tools"

      jsonText `shouldNotContain` "instructions"

      rpcWrapped `shouldNotContain` "logging"
      rpcWrapped `shouldNotContain` "completions"
      rpcWrapped `shouldNotContain` "prompts"
      rpcWrapped `shouldNotContain` "resources"
      rpcWrapped `shouldNotContain` "tools"

      rpcWrapped `shouldNotContain` "instructions"

  describe "InitializeResponse decoding" $ do
    let serverCaps = ServerCapabilities Nothing Nothing Nothing Nothing Nothing
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeResponse serverCaps implementation Nothing

    let marshalled = "{\"capabilities\":{},\"protocolVersion\":\"2025-03-26\",\"serverInfo\":{\"name\":\"haskell-network-mcp-test\",\"version\":\"v0.0.0.1\"}}"

    it "decodes" $ do
      decode marshalled `shouldBe` Just subject

  describe "InitializedNotification encoding" $ do
    it "is null" $ do
      let subject = InitializedNotification
      let json    = encode subject
      let jsonText   = TL.unpack $ encodeToLazyText subject
      C.unpack json `shouldBe` "null"
      jsonText `shouldBe` "null"
