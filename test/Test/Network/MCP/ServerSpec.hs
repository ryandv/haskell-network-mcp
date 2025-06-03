{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.MCP.ServerSpec where

import Conduit

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import GHC.Generics

import Network.MCP.Server
import Network.MCP.Types

import Test.Hspec

sourceAndWait    :: MonadIO m => [B.ByteString] -> ConduitT () B.ByteString m ()
sourceAndWait bs = do
  yieldMany bs
  liftIO $ threadDelay 50000 -- keep the source open long enough to receive a response

initializeResultDecoder :: (MonadIO m, MonadLogger m) => B.ByteString -> m ()
initializeResultDecoder = liftIO . (either (expectationFailure . ("failed to decode InitializeResult: " ++)) (const $ return ())
                                 . ((parseEither (either fail (withObject "InitializeResult" (.: "result")))) :: Either String Value -> Either String InitializeResult)
                                 . (eitherDecode                                                              :: L.ByteString -> Either String Value)
                                 . L.fromStrict)
  where printRPCPayload :: (MonadIO m, MonadLogger m, Show a) => a -> m a
        printRPCPayload = liftA2 (>>) (liftA2 (>>) (liftIO . print) (const . liftIO $ print "=====\n")) (return)

callToolResultDecoder          :: (MonadIO m, MonadLogger m) => TL.Text -> B.ByteString -> m ()
callToolResultDecoder expected = liftIO . (maybe (expectationFailure "failed to decode CallToolResult") ((`shouldBe` (V.fromList [TextContent (TL.toStrict expected) Nothing])) . content)
                               . ((parseMaybe (maybe mempty (withObject "CallToolResult" (.: "result")))) :: Maybe Value -> Maybe CallToolResult)
                               . (decode                                                                  :: L.ByteString -> Maybe Value)
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

data ClientState = ClientStart | ClientInitializing | ClientOperational

echoClient s = do
  st <- liftIO . atomically . readTVar $ s
  case st of
    ClientStart        -> ((linesUnboundedAsciiC .| await) >>= (maybe (return ()) initializeResultDecoder)) >> (liftIO . atomically . swapTVar s $ ClientOperational)
    ClientOperational  -> ((linesUnboundedAsciiC .| await) >>= (maybe (return ()) (callToolResultDecoder "hello world"))) >> (return ClientOperational)
  return ()

spec :: Spec
spec = describe "MCP server" $ do
  let implementation = Implementation "haskell-network-mcp-test" "v0.0.0.1"
  let clientCaps  = ClientCapabilities Nothing Nothing
  let initreq     = InitializeRequest clientCaps implementation
  let initReqJSON = L.toStrict . encode $ mcpRequestJSON (Just $ Left 0) initreq
  let initNotJSON = L.toStrict . encode $ mcpRequestJSON (Nothing) InitializedNotification
  let initJSONs   = [initReqJSON, initNotJSON]

  -- NOTE: these specs can pass vacuously if the sink is empty,
  --       (e.g. the stream is closed before we have a chance to
  --       read anything from it) as no expectations will have
  --       been set...
  it "can initialize a connection" $ do
    let input       = sourceAndWait initJSONs
    let output      = takeC 1 .| mapM_C initializeResultDecoder

    liftIO . runNoLoggingT $ server input output []

    -- TODO: strengthen expectation in light of above note
    True `shouldBe` True

  it "accepts user-defined tool call handlers" $ do
    let callToolReq   = CallToolRequest "echo" . Just $ M.fromList [("text", "hello world")]
    let callToolJSON  = L.toStrict . encode $ mcpRequestJSON (Just $ Left 0) callToolReq

    let input         = sourceAndWait $ [initReqJSON, initNotJSON, callToolJSON]

    let echoConfig = toolBuilder "echo"
                                 "Responds with its input"
                                 [ ToolArgumentDescriptor "text"
                                                          "Arbitrary text to be returned to the client"
                                                          "string"
                                                          True
                                 ]
                                 echoHandler

    s <- liftIO . atomically . newTVar $ ClientStart
    liftIO . runNoLoggingT $ server input (fuck s) [ echoConfig ]

    -- TODO: strengthen expectation in light of above note
    True `shouldBe` True

  -- TODO: test coverage for tool not found; should return -32602 "Unknown tool: $TOOL_NAME"
  -- TODO: test coverage for invalid tool arguments; should return -32602?
