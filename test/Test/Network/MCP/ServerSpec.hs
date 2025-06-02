{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.MCP.ServerSpec where

import Conduit

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import GHC.Generics

import Network.MCP.Server
import Network.MCP.Types
import Network.JSONRPC

import Test.Hspec

sourceForever    :: MonadIO m => [B.ByteString] -> ConduitT () B.ByteString m ()
sourceForever bs = do
  yieldMany bs
  liftIO $ threadDelay 50000 -- keep the source open long enough to receive a response

initializeResultDecoder :: (MonadIO m, MonadLogger m) => B.ByteString -> m ()
initializeResultDecoder = liftIO . (maybe (expectationFailure "failed to decode InitializeResult") (const $ return ())
                                 . ((>>= fromResponse methodInitialize) :: Maybe Response -> Maybe InitializeResult)
                                 . (decode                              :: L.ByteString -> Maybe Response)
                                 . L.fromStrict)
  where printRPCPayload :: (MonadIO m, MonadLogger m, Show a) => a -> m a
        printRPCPayload = liftA2 (>>) (liftA2 (>>) (liftIO . print) (const . liftIO $ print "=====\n")) (return)

callToolResultDecoder          :: (MonadIO m, MonadLogger m) => TL.Text -> B.ByteString -> m ()
callToolResultDecoder expected = liftIO . (maybe (expectationFailure "failed to decode CallToolResult") ((`shouldBe` (V.fromList [TextContent (TL.toStrict expected) Nothing])) . content)
                               . ((>>= fromResponse methodToolsCall) :: Maybe Response -> Maybe CallToolResult)
                               . (decode                             :: L.ByteString -> Maybe Response)
                               . L.fromStrict)
  where printRPCPayload :: (MonadIO m, MonadLogger m, Show a) => a -> m a
        printRPCPayload = liftA2 (>>) (liftA2 (>>) (liftIO . print) (const . liftIO $ print "=====\n")) (return)

data EchoArguments = EchoArguments
  { text :: T.Text
  } deriving(Eq, Generic, Show)

instance FromJSON EchoArguments
instance ToJSON EchoArguments where
  toEncoding = genericToEncoding defaultOptions

-- TODO: annotation support
echoHandler :: (MonadIO m, MonadLogger m) => CallToolRequest -> MCPT m (Either ToolError CallToolResult)
echoHandler = handleToolCall (\(EchoArguments txt) -> return . Right $ CallToolResult (V.fromList [TextContent txt Nothing]) (Just False))

spec :: Spec
spec = describe "MCP server" $ do
  let implementation = Implementation "haskell-network-mcp-test" "v0.0.0.1"
  let clientCaps  = ClientCapabilities Nothing Nothing
  let initreq     = InitializeRequest clientCaps implementation
  let initReqJSON = L.toStrict . encode $ buildRequest V2 initreq (IdInt 0)
  let initNotJSON = L.toStrict . encode $ buildRequest V2 InitializedNotification (IdInt (-1))
  let initJSONs   = [initReqJSON, initNotJSON]

  -- NOTE: these specs can pass vacuously if the sink is empty,
  --       (e.g. the stream is closed before we have a chance to
  --       read anything from it) as no expectations will have
  --       been set...
  it "can initialize a connection" $ do
    let input       = sourceForever initJSONs
    let output      = takeC 1 .| mapM_C initializeResultDecoder

    liftIO . runNoLoggingT $ server input output [] []

    -- TODO: strengthen expectation in light of above note
    True `shouldBe` True

  it "accepts user-defined tool call handlers" $ do
    let callToolReq   = CallToolRequest "echo" . Just $ M.fromList [("text", "hello world")]
    let callToolJSON  = L.toStrict . encode $ buildRequest V2 callToolReq (IdInt 0)

    let input         = sourceForever $ initJSONs ++ [callToolJSON]
    let output        = takeC 1 .| mapM_C initializeResultDecoder >> takeC 1 .| mapM_C (callToolResultDecoder "hello world")

    let echoConfig = toolBuilder "echo"
                                 "Responds with its input"
                                 [ ToolArgumentDescriptor "text"
                                                          "Arbitrary text to be returned to the client"
                                                          "string"
                                                          True
                                 ]
                                 echoHandler

    liftIO . runNoLoggingT $ server input output [] [ echoConfig ]

    -- TODO: strengthen expectation in light of above note
    True `shouldBe` True

  -- TODO: test coverage for tool not found; should return -32602 "Unknown tool: $TOOL_NAME"
  -- TODO: test coverage for invalid tool arguments; should return -32602?
