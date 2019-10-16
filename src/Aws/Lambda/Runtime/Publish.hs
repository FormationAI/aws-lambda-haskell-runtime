{-| Publishing of results/errors back to the
AWS Lambda runtime API -}
module Aws.Lambda.Runtime.Publish
  ( result
  , invocationError
  , parsingError
  , runtimeInitError
  ) where

import Control.Monad (void)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as Http

import qualified Aws.Lambda.Runtime.API.Endpoints as Endpoints
import Aws.Lambda.Runtime.Context (Context (..))
import qualified Aws.Lambda.Runtime.Error as Error

-- | Publishes the result back to AWS Lambda
result :: Text -> String -> Context -> Http.Manager -> IO ()
result res lambdaApi context manager = do
  let Endpoints.Endpoint endpoint = Endpoints.response lambdaApi (awsRequestId context)
  rawRequest <- Http.parseRequest endpoint
  let request = rawRequest
                { Http.method = "POST"
                , Http.requestBody = Http.RequestBodyBS (TE.encodeUtf8 res)
                }
  void $ Http.httpNoBody request manager

-- | Publishes an invocation error back to AWS Lambda
invocationError :: Error.Invocation -> String -> Context -> Http.Manager -> IO ()
invocationError err lambdaApi context =
  publish err (Endpoints.invocationError lambdaApi $ awsRequestId context)
    context

-- | Publishes a parsing error back to AWS Lambda
parsingError :: Error.Parsing -> String -> Context -> Http.Manager -> IO ()
parsingError err lambdaApi context =
  publish err (Endpoints.invocationError lambdaApi $ awsRequestId context)
    context

-- | Publishes a runtime initialization error back to AWS Lambda
runtimeInitError :: ToJSON err => err -> String -> Context -> Http.Manager -> IO ()
runtimeInitError err lambdaApi =
  publish err (Endpoints.runtimeInitError lambdaApi)

publish :: ToJSON err => err -> Endpoints.Endpoint -> Context -> Http.Manager -> IO ()
publish err (Endpoints.Endpoint endpoint) Context{..} manager = do
  rawRequest <- Http.parseRequest endpoint
  let request = rawRequest
                { Http.method = "POST"
                , Http.requestBody = Http.RequestBodyLBS (encode err)
                }
  void $ Http.httpNoBody request manager
