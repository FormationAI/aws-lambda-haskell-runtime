module Aws.Lambda.Runtime
  ( runLambda
  , Handler
  , Event(..)
  , Runtime.LambdaResult(..)
  ) where

import Control.Exception.Safe.Checked
import Control.Monad (forever)
import qualified Network.HTTP.Client as Http

import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.Publish as Publish

type Handler =
  Event -> IO (Either Text Text)

data Event =
  Event {
      memoryLimitInMb    :: !Int
    , functionName       :: !Text
    , functionVersion    :: !Text
    , invokedFunctionArn :: !Text
    , awsRequestId       :: !Text
    , xrayTraceId        :: !Text
    , logStreamName      :: !Text
    , logGroupName       :: !Text
    , deadlineMs         :: !Int
    , handlerName        :: !Text
    , clientContext      :: !Text
    , event              :: !Text
    }

packEvent :: Text -> Context.Context -> ApiInfo.Event -> Event
packEvent h ctx event =
  Event {
      memoryLimitInMb    = Context.memoryLimitInMb ctx
    , functionName       = T.pack (Context.functionName ctx)
    , functionVersion    = T.pack (Context.functionVersion ctx)
    , invokedFunctionArn = T.pack (Context.invokedFunctionArn ctx)
    , awsRequestId       = T.pack (Context.awsRequestId ctx)
    , xrayTraceId        = T.pack (Context.xrayTraceId ctx)
    , logStreamName      = T.pack (Context.logStreamName ctx)
    , logGroupName       = T.pack (Context.logGroupName ctx)
    , deadlineMs         = ApiInfo.deadlineMs event
    , handlerName        = h
    , clientContext      = TE.decodeUtf8 (ApiInfo.clientContext event)
    , event              = TE.decodeUtf8 (LB.toStrict (ApiInfo.event event))
    }

-- | Runs the user @haskell_lambda@ executable and posts back the
-- results. This is called from the layer's @main@ function.
runLambda :: Handler -> IO ()
runLambda handler = do
  manager <- Http.newManager httpManagerSettings
  forever $ do
    lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
    event     <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing
    context   <- Context.initialize event `catch` errorParsing `catch` variableNotSet
    ((invokeAndRun handler manager lambdaApi event context
      `catch` \err -> Publish.parsingError err lambdaApi context manager)
      `catch` \err -> Publish.invocationError err lambdaApi context manager)
      `catch` \(err :: Error.EnvironmentVariableNotSet) -> Publish.runtimeInitError err lambdaApi context manager

httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
  { Http.managerResponseTimeout = Http.responseTimeoutNone
  }

invokeAndRun
  :: Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Handler
  -> Http.Manager
  -> String
  -> ApiInfo.Event
  -> Context.Context
  -> IO ()
invokeAndRun handler manager lambdaApi event context = do
  result    <- invokeWithHandler handler event context
  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

invokeWithHandler
  :: Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Handler
  -> ApiInfo.Event
  -> Context.Context
  -> IO Text
invokeWithHandler handler event context = do
  handlerName <- fmap T.pack Environment.handlerName
  result <- handler (packEvent handlerName context event)
  case result of
    Left err ->
      throw $ Error.Invocation (T.unpack err)
    Right value ->
      pure value

variableNotSet :: Error.EnvironmentVariableNotSet -> IO a
variableNotSet (Error.EnvironmentVariableNotSet env) =
  error ("Error initializing, variable not set: " <> env)

errorParsing :: Error.Parsing -> IO a
errorParsing Error.Parsing{..} =
  error ("Failed parsing " <> errorMessage <> ", got" <> actualValue)
