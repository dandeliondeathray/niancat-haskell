module Matchers
  ( allOf
  , atLeastOneOf
  , exactly
  ) where

import           Web

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Test.Hspec.Wai

instance FromJSON Message where
  parseJSON =
    withObject "message" $ \o -> do
      responseType <- o .: "response_type"
      message <- o .: "message"
      case responseType of
        "notification" -> return $ Notification message
        "reply"        -> return $ Reply message
        _              -> fail ("Invalid message type " ++ responseType)

makeBodyMatcher :: (Body -> Maybe String) -> ResponseMatcher
makeBodyMatcher matcher =
  ResponseMatcher
    { matchStatus = 200
    , matchHeaders = []
    , matchBody = MatchBody (\_ body -> matcher body)
    }

allOf :: [Message] -> ResponseMatcher
allOf msgs = makeBodyMatcher (bodyMatcher "all of" ok msgs)
  where
    ok :: [Message] -> Bool
    ok actual = all (`elem` actual) msgs

atLeastOneOf :: [Message] -> ResponseMatcher
atLeastOneOf msgs = makeBodyMatcher (bodyMatcher "any of" ok msgs)
  where
    ok :: [Message] -> Bool
    ok actual = any (`elem` actual) msgs

exactly :: [Message] -> ResponseMatcher
exactly msgs = makeBodyMatcher (bodyMatcher "exactly" ok msgs)
  where
    ok actual = msgs == actual

bodyMatcher ::
     String -> ([Message] -> Bool) -> [Message] -> Body -> Maybe String
bodyMatcher label ok msgs body =
  case eitherDecode body of
    Right actual -> messagesMissing label msgs actual <$ guard (not $ ok actual)
    Left err -> Just err

messagesMissing :: String -> [Message] -> [Message] -> String
messagesMissing allOrSome expected actual =
  unlines
    [ "some expected messages were missing"
    , "  expected " ++ allOrSome ++ ": " ++ show expected
    , "          but got: " ++ show actual
    , "          missing: " ++
      (show . filter (not . flip elem actual) $ expected)
    , "            extra: " ++
      (show . filter (not . flip elem expected) $ actual)
    ]
