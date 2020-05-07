module Errors where

import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as LB
import           Data.HashMap.Strict
import           Data.IORef
import           Data.List
import           Data.String.Conversions (cs)
import           Network.HTTP.Types
import           Network.Wai

errorsAsJson :: Middleware
errorsAsJson baseApp req respond = baseApp req respond'
 where
  respond' response = do
    body <- responseBody response
    let
      response'   = createErrorResponse status body
      status      = responseStatus response
      contentType = find ((hContentType ==) . fst) . responseHeaders $ response
    case (status, contentType) of
      (s, Nothing) | statusIsClientError s || statusIsServerError s ->
        respond response'
      _ -> respond response


createErrorResponse :: Status -> LB.ByteString -> Response
createErrorResponse status body = responseLBS status headers body'
 where
  Status code msg = status
  headers         = [(hContentType, "application/json")]
  body'           = encode . Object $ fromList
    [ ("status" , toJSON code)
    , ("message", String $ if body == mempty then cs msg else cs body)
    ]

responseBody :: Response -> IO LB.ByteString
responseBody res =
  let (_status, _headers, streamBody) = responseToStream res
  in  streamBody $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        builder <- readIORef content
        return . toLazyByteString $ builder
