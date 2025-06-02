{-# LANGUAGE OverloadedStrings #-}
module Test.Network.MCP.TypesSpec where

import qualified Prelude
import Prelude hiding(id)

import Control.Monad.IO.Class

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Maybe
import Data.UUID
import Data.UUID.V4
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import Network.MCP.Types
import Network.JSONRPC

import Test.Hspec

-- because aeson's generics are inscrutable in combination with Network.JSONRPC...
spec :: Spec
spec = context "json marshalling" $ do
  let implementation = Implementation "haskell-network-mcp-test" "v0.0.0.1"
  let uuid       = Just $ Right "550e8400-e29b-41d4-a716-446655440000"

  context "JSON-RPC" $ do
    describe "JSONRPCRequest decoding" $ do
      it "decodes" $ do
        let example = "{\"id\":1,\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{\"capabilities\":{},\"clientInfo\":{\"name\":\"test\",\"version\":\"1\"},\"protocolVersion\":\"2025-03-26\"}}"
        let d = eitherDecodeStrictText :: T.Text -> Either String JSONRPCRequest
        let expected = JSONRPCRequest { method = "initialize"
                                      , params = Just $ object [ "capabilities" .= object []
                                                               , "clientInfo"   .= object [ "name" .= ("test" :: T.Text), "version" .= ("1" :: T.Text) ]
                                                               , "protocolVersion" .= ("2025-03-26" :: T.Text)
                                                               ]
                                      , id = Just $ Left 1
                                      }

        let decoded = d example

        decoded `shouldBe` (Right expected)

  describe "InitializeRequest encoding" $ do
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeRequest clientCaps implementation

    let json       = encode subject
    let jsonText   = TL.unpack $ encodeToLazyText subject
    let req        = buildRequest V2 subject (IdInt 1)
    let rpcWrapped = TL.unpack $ encodeToLazyText req
    let mpcRequest = TL.unpack . encodeToLazyText . mcpRequestJSON uuid $ subject

    it "includes a protocolVersion" $ do
      mpcRequest    `shouldContain` "2025-03-26"

    it "does not encode capabilities it does not have" $ do
      C.unpack json `shouldNotContain` "roots"
      C.unpack json `shouldNotContain` "sampling"
      jsonText      `shouldNotContain` "roots"
      jsonText      `shouldNotContain` "sampling"
      mpcRequest    `shouldNotContain` "roots"
      mpcRequest    `shouldNotContain` "sampling"
      -- [0] the rpcWrapped examples will fail when using
      -- generics presumably because of how the underlying
      -- payload is wrapped by Network.JSONRPC's
      -- Request/Response types; something is causing
      -- omitNothingFields to not be respected
      rpcWrapped    `shouldNotContain` "roots"
      rpcWrapped    `shouldNotContain` "sampling"

  describe "InitializeRequest decoding" $ do
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeRequest clientCaps implementation

    let marshalled = "{\"capabilities\":{},\"clientInfo\":{\"name\":\"haskell-network-mcp-test\",\"version\":\"v0.0.0.1\"},\"protocolVersion\":\"2025-03-26\"}"

    it "decodes" $ do
      decode marshalled `shouldBe` Just subject

  describe "InitializeResult encoding" $ do
    let serverCaps = ServerCapabilities Nothing Nothing Nothing Nothing Nothing
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeResult serverCaps implementation Nothing

    let json       = encode subject
    let jsonText   = TL.unpack $ encodeToLazyText subject
    res            <- buildResponse ((const (return . Right $ subject)) :: (Monad m) => InitializeRequest -> m (Either ErrorObj InitializeResult)) $ buildRequest V2 (InitializeRequest clientCaps implementation) (IdInt 1)
    let rpcWrapped = TL.unpack $ encodeToLazyText $ res
    let mpcRequest = TL.unpack . encodeToLazyText . mcpResultJSON uuid $ subject

    it "includes a protocolVersion" $ do
      mpcRequest    `shouldContain` "2025-03-26"

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

      mpcRequest `shouldNotContain` "logging"
      mpcRequest `shouldNotContain` "completions"
      mpcRequest `shouldNotContain` "prompts"
      mpcRequest `shouldNotContain` "resources"
      mpcRequest `shouldNotContain` "tools"

      mpcRequest `shouldNotContain` "instructions"

      -- see [0] above re. omitNothingFields
      rpcWrapped `shouldNotContain` "logging"
      rpcWrapped `shouldNotContain` "completions"
      rpcWrapped `shouldNotContain` "prompts"
      rpcWrapped `shouldNotContain` "resources"
      rpcWrapped `shouldNotContain` "tools"

      rpcWrapped `shouldNotContain` "instructions"

  describe "InitializeResult decoding" $ do
    let serverCaps = ServerCapabilities Nothing Nothing Nothing Nothing Nothing
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeResult serverCaps implementation Nothing

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

  describe "CallToolRequest encoding" $ do
    it "encodes" $ do
      let subject = CallToolRequest "foobar" Nothing

      let json = encode subject
      let jsonText = TL.unpack $ encodeToLazyText subject
      let req = buildRequest V2 subject (IdInt 1)
      let rpcWrapped = TL.unpack $ encodeToLazyText req
      let mpcRequest = TL.unpack . encodeToLazyText . mcpRequestJSON uuid $ subject

      C.unpack json `shouldBe` "{\"name\":\"foobar\"}"
      jsonText      `shouldBe` "{\"name\":\"foobar\"}"
      mpcRequest    `shouldBe` "{\"id\":\"550e8400-e29b-41d4-a716-446655440000\",\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"foobar\"}}"
      -- see [0] above re. omitNothingFields
      rpcWrapped    `shouldBe` "{\"id\":1,\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"foobar\"}}"

    it "encodes optional arguments" $ do
      let subject = CallToolRequest "foobar" (Just $ M.fromList [("foo", "bar")])
      let json = encode subject
      let mpcRequest = TL.unpack . encodeToLazyText . mcpRequestJSON uuid $ subject
      C.unpack json `shouldBe` "{\"name\":\"foobar\",\"arguments\":{\"foo\":\"bar\"}}"
      mpcRequest    `shouldBe` "{\"id\":\"550e8400-e29b-41d4-a716-446655440000\",\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"arguments\":{\"foo\":\"bar\"},\"name\":\"foobar\"}}"

  describe "CallToolRequest decoding" $ do
    it "decodes" $ do
      let subject = CallToolRequest "echo" . Just $ M.fromList [("text", "hello world")]

      let marshalled = "{\"arguments\":{\"text\":\"hello world\"},\"name\":\"echo\"}"
      decode marshalled `shouldBe` Just subject

    it "decodes when wrapped in a JSONRPC Request" $ do
      let subject    = CallToolRequest "echo" . Just $ M.fromList [("text", "hello world")]
      let params     = (Object (M.fromList [("arguments", Object (M.fromList [("text", String "hello world")])), ("name", String "echo")]))
      let rpcWrapped = buildRequest V2 params (IdInt 1)
      let request    = Request { getReqVer = V2
                               , getReqMethod = "tools/call"
                               , getReqParams = Object (M.fromList [("arguments",Object (M.fromList [("text",String "hello world")])),("name",String "echo")])
                               , getReqId = IdInt 0
                               }

      fromRequest rpcWrapped `shouldBe` Right subject
      let maybeParser = parseParams methodToolsCall :: Maybe (Value -> Parser CallToolRequest)
      let doParse     = flip parseEither params :: (Value -> Parser CallToolRequest) -> Either String CallToolRequest
      (case maybeParser of
        Nothing -> Left $ "fuck"
        Just p -> case parseEither p params of
          Left e -> Left $ "shit"
          Right q -> Right q) `shouldBe` Right subject


  describe "CallToolResult encoding" $ do
    context "CallToolResult proper" $ do
      it "encodes" $ do
        let subject    = CallToolResult (V.singleton (TextContent "hello world" Nothing)) Nothing

        let json       = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject
        res            <- buildResponse ((const (return . Right $ subject)) :: (Monad m) => CallToolRequest -> m (Either ErrorObj CallToolResult)) $ buildRequest V2 (CallToolRequest "foobar" Nothing) (IdInt 1)
        let rpcWrapped = TL.unpack $ encodeToLazyText res
        let mpcRequest = TL.unpack . encodeToLazyText . mcpResultJSON uuid $ subject

        C.unpack json `shouldContain` "\"text\":\"hello world\""
        jsonText      `shouldContain` "\"text\":\"hello world\""
        mpcRequest    `shouldContain` "\"text\":\"hello world\""
        rpcWrapped    `shouldContain` "\"text\":\"hello world\""

        C.unpack json `shouldContain` "\"type\":\"text\""
        jsonText      `shouldContain` "\"type\":\"text\""
        mpcRequest    `shouldContain` "\"type\":\"text\""
        rpcWrapped    `shouldContain` "\"type\":\"text\""

        C.unpack json `shouldNotContain` "\"isError\""
        jsonText      `shouldNotContain` "\"isError\""
        mpcRequest    `shouldNotContain` "\"isError\""
        rpcWrapped    `shouldNotContain` "\"isError\""

      it "encodes errors" $ do
        let subject = CallToolResult (V.singleton (TextContent "hello world" Nothing)) (Just True)

        let json = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject
        res            <- buildResponse ((const (return . Right $ subject)) :: (Monad m) => CallToolRequest -> m (Either ErrorObj CallToolResult)) $ buildRequest V2 (CallToolRequest "foobar" Nothing) (IdInt 1)
        let rpcWrapped = TL.unpack $ encodeToLazyText res
        let mpcRequest = TL.unpack . encodeToLazyText . mcpResultJSON uuid $ subject

        C.unpack json `shouldContain` "\"isError\":true"
        jsonText      `shouldContain` "\"isError\":true"
        mpcRequest    `shouldContain` "\"isError\":true"
        rpcWrapped    `shouldContain` "\"isError\":true"

    context "CallToolResultContent" $ do
      it "encodes TextContent" $ do
        let subject = TextContent "hello world" (Just $ Annotations (Just $ V.fromList [User]) (Just 42))

        let json       = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject
        res            <- buildResponse ((const (return . Right $ CallToolResult (V.singleton subject) Nothing)) :: (Monad m) => CallToolRequest -> m (Either ErrorObj CallToolResult)) $ buildRequest V2 (CallToolRequest "foobar" Nothing) (IdInt 1)
        let rpcWrapped = TL.unpack $ encodeToLazyText res

        C.unpack json `shouldContain` "\"audience\":[\"user\"]"
        jsonText      `shouldContain` "\"audience\":[\"user\"]"
        rpcWrapped    `shouldContain` "\"audience\":[\"user\"]"

        C.unpack json `shouldContain` "\"priority\":42"
        jsonText      `shouldContain` "\"priority\":42"
        rpcWrapped    `shouldContain` "\"priority\":42"

        C.unpack json `shouldContain` "\"text\":\"hello world\""
        jsonText      `shouldContain` "\"text\":\"hello world\""
        rpcWrapped    `shouldContain` "\"text\":\"hello world\""

        C.unpack json `shouldContain` "\"type\":\"text\""
        jsonText      `shouldContain` "\"type\":\"text\""
        rpcWrapped    `shouldContain` "\"type\":\"text\""

      it "encodes TextContent without annotations" $ do
        let subject    = TextContent "hello world" Nothing

        let json       = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject
        res            <- buildResponse ((const (return . Right $ CallToolResult (V.singleton subject) Nothing)) :: (Monad m) => CallToolRequest -> m (Either ErrorObj CallToolResult)) $ buildRequest V2 (CallToolRequest "foobar" Nothing) (IdInt 1)
        let rpcWrapped = TL.unpack $ encodeToLazyText res

        C.unpack json `shouldNotContain` "\"audience\""
        jsonText      `shouldNotContain` "\"audience\""
        rpcWrapped    `shouldNotContain` "\"audience\""

        C.unpack json `shouldNotContain` "\"priority\""
        jsonText      `shouldNotContain` "\"priority\""
        rpcWrapped    `shouldNotContain` "\"priority\""

        C.unpack json `shouldNotContain` "\"annotations\""
        jsonText      `shouldNotContain` "\"annotations\""
        rpcWrapped    `shouldNotContain` "\"annotations\""

  describe "CallToolResult decoding" $ do
    context "CallToolResultContent" $ do
      it "decodes TextContent" $ do
        let subject = TextContent "hello world" (Just $ Annotations (Just $ V.fromList [User]) (Just 42))
        let marshalled = "{\"annotations\":{\"audience\":[\"user\"],\"priority\":42},\"text\":\"hello world\",\"type\":\"text\"}"
        decode marshalled `shouldBe` Just subject

      it "decodes TextContent without annotations" $ do
        let subject = TextContent "hello world" Nothing
        let marshalled = "{\"text\":\"hello world\",\"type\":\"text\"}"
        decode marshalled `shouldBe` Just subject

      it "decodes ImageContent" $ do
        let subject = ImageContent "AAABAAMAMD==" "image/x-icon" Nothing
        let marshalled = "{\"mimeType\":\"image/x-icon\",\"type\":\"image\",\"data\":\"AAABAAMAMD==\"}"
        decode marshalled `shouldBe` Just subject
