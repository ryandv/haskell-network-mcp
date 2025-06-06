{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.MCP.TypesSpec where

import qualified Prelude
import Prelude hiding(id)

import Control.Monad.IO.Class

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Either
import Data.Maybe
import Data.UUID
import Data.UUID.V4
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import Network.MCP.Types

import Test.Hspec

spec :: Spec
spec = context "json marshalling" $ do
  let implementation = Implementation "haskell-network-mcp-test" "v0.0.0.1"
  let uuid       = Just $ Right "550e8400-e29b-41d4-a716-446655440000"

  context "JSON-RPC" $ do
    describe "JSONRPCRequest encoding" $ do
      it "encodes" $ do
        let subject = JSONRPCRequest { method = "initialize"
                                     , params = Just $ object [ "capabilities" .= object []
                                                              , "clientInfo"   .= object [ "name" .= ("test" :: T.Text), "version" .= ("1" :: T.Text) ]
                                                              , "protocolVersion" .= ("2025-03-26" :: T.Text)
                                                              ]
                                     , id = Just $ Left 1
                                     }
        let marshalled = encode subject

        marshalled `shouldBe` "{\"method\":\"initialize\",\"jsonrpc\":\"2.0\",\"params\":{\"capabilities\":{},\"clientInfo\":{\"name\":\"test\",\"version\":\"1\"},\"protocolVersion\":\"2025-03-26\"},\"id\":1}"
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

      it "fails to decode requests missing the jsonrpc 2.0 property" $ do
        let example = "{\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":{},\"clientInfo\":{\"name\":\"test\",\"version\":\"1\"},\"protocolVersion\":\"2025-03-26\"}}"
        let d = eitherDecodeStrictText :: T.Text -> Either String JSONRPCRequest

        let decoded = d example

        isLeft decoded `shouldBe` True

  describe "InitializeRequest encoding" $ do
    let clientCaps = ClientCapabilities Nothing Nothing
    let subject    = InitializeRequest clientCaps implementation

    let json       = encode subject
    let jsonText   = TL.unpack $ encodeToLazyText subject
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

  describe "ListToolsResult encoding" $ do
    it "encodes" $ do
      let tool = Tool { name = "echo"
                      , description = Just ("returns its input" :: T.Text)
                      , inputSchema = InputSchema (Just . M.fromList $ [ "text" .= object [ "type"        .= ("string" :: T.Text)
                                                                                          , "description" .= ("input to be returned as output" :: T.Text)
                                                                                          ]
                                                                       ])
                                                  (Just . V.fromList $ ["echo"])
                      , annotations = Just $ ToolAnnotations { title = "echo tool"
                                                             , readOnlyHint = Just True
                                                             , destructiveHint = Just False
                                                             , idempotentHint = Just True
                                                             , openWorldHint = Just False
                                                             }
                      }
      let subject = ListToolsResult . V.fromList $ [tool]

      let json = encode subject

      C.unpack json `shouldBe` "{\"tools\":[{\"name\":\"echo\",\"description\":\"returns its input\",\"inputSchema\":{\"properties\":{\"text\":{\"description\":\"input to be returned as output\",\"type\":\"string\"}},\"required\":[\"echo\"],\"type\":\"object\"},\"annotations\":{\"title\":\"echo tool\",\"readOnlyHint\":true,\"destructiveHint\":false,\"idempotentHint\":true,\"openWorldHint\":false}}]}"

    it "omits absent annotations" $ do
      let tool = Tool { name = "echo"
                      , description = Just ("returns its input" :: T.Text)
                      , inputSchema = InputSchema (Just . M.fromList $ [ "text" .= object [ "type"        .= ("string" :: T.Text)
                                                                                          , "description" .= ("input to be returned as output" :: T.Text)
                                                                                          ]
                                                                       ])
                                                  (Just . V.fromList $ ["echo"])
                      , annotations = Nothing
                      }
      let subject = ListToolsResult . V.fromList $ [tool]

      let json = encode subject

      C.unpack json `shouldBe` "{\"tools\":[{\"name\":\"echo\",\"description\":\"returns its input\",\"inputSchema\":{\"properties\":{\"text\":{\"description\":\"input to be returned as output\",\"type\":\"string\"}},\"required\":[\"echo\"],\"type\":\"object\"}}]}"

  describe "CallToolRequest encoding" $ do
    it "encodes" $ do
      let subject = CallToolRequest "foobar" Nothing

      let json = encode subject
      let jsonText = TL.unpack $ encodeToLazyText subject
      let mpcRequest = TL.unpack . encodeToLazyText . mcpRequestJSON uuid $ subject

      C.unpack json `shouldBe` "{\"name\":\"foobar\"}"
      jsonText      `shouldBe` "{\"name\":\"foobar\"}"
      mpcRequest    `shouldBe` "{\"id\":\"550e8400-e29b-41d4-a716-446655440000\",\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"foobar\"}}"

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

  describe "CallToolResult encoding" $ do
    context "CallToolResult proper" $ do
      it "encodes" $ do
        let subject    = CallToolResult (V.singleton (TextContent "hello world" Nothing)) Nothing

        let json       = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject
        let mpcRequest = TL.unpack . encodeToLazyText . mcpResultJSON uuid $ subject

        C.unpack json `shouldContain` "\"text\":\"hello world\""
        jsonText      `shouldContain` "\"text\":\"hello world\""
        mpcRequest    `shouldContain` "\"text\":\"hello world\""

        C.unpack json `shouldContain` "\"type\":\"text\""
        jsonText      `shouldContain` "\"type\":\"text\""
        mpcRequest    `shouldContain` "\"type\":\"text\""

        C.unpack json `shouldNotContain` "\"isError\""
        jsonText      `shouldNotContain` "\"isError\""
        mpcRequest    `shouldNotContain` "\"isError\""

      it "encodes errors" $ do
        let subject = CallToolResult (V.singleton (TextContent "hello world" Nothing)) (Just True)

        let json = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject
        let mpcRequest = TL.unpack . encodeToLazyText . mcpResultJSON uuid $ subject

        C.unpack json `shouldContain` "\"isError\":true"
        jsonText      `shouldContain` "\"isError\":true"
        mpcRequest    `shouldContain` "\"isError\":true"

    context "CallToolResultContent" $ do
      it "encodes TextContent" $ do
        let subject = TextContent "hello world" (Just $ Annotations (Just $ V.fromList [User]) (Just 42))

        let json       = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject

        C.unpack json `shouldContain` "\"audience\":[\"user\"]"
        jsonText      `shouldContain` "\"audience\":[\"user\"]"

        C.unpack json `shouldContain` "\"priority\":42"
        jsonText      `shouldContain` "\"priority\":42"

        C.unpack json `shouldContain` "\"text\":\"hello world\""
        jsonText      `shouldContain` "\"text\":\"hello world\""

        C.unpack json `shouldContain` "\"type\":\"text\""
        jsonText      `shouldContain` "\"type\":\"text\""

      it "encodes TextContent without annotations" $ do
        let subject    = TextContent "hello world" Nothing

        let json       = encode subject
        let jsonText   = TL.unpack $ encodeToLazyText subject

        C.unpack json `shouldNotContain` "\"audience\""
        jsonText      `shouldNotContain` "\"audience\""

        C.unpack json `shouldNotContain` "\"priority\""
        jsonText      `shouldNotContain` "\"priority\""

        C.unpack json `shouldNotContain` "\"annotations\""
        jsonText      `shouldNotContain` "\"annotations\""

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
