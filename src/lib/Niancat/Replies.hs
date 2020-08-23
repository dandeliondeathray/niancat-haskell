module Niancat.Replies where

import Data.Aeson
import Data.Text
import TextShow

import Niancat.Domain

data Message
  = Notification Text
  | Reply Text
  deriving (Show, Eq)

instance ToJSON Message where
  toJSON (Notification text) =
    object ["response_type" .= ("notification" :: Text), "message" .= text]
  toJSON (Reply text) =
    object ["response_type" .= ("reply" :: Text), "message" .= text]

class Response r where
  messages :: r -> [Message]

instance Response NiancatEvent where
  messages SolutionSubmittedWithNoPuzzleSet = [Reply "Nian är inte satt än!"]
  messages (IncorrectSolutionSubmitted guess) = [Reply $ "Ordet " <> showt guess <> " finns inte med i SAOL."]
  messages (CorrectSolutionSubmitted guess _ _) = [Reply $ "Ordet " <> showt guess <> " är korrekt!"]
  messages (PuzzleSet p) = [Reply "OK!", Notification $ mconcat ["Dagens nia är **", showt p, "**"]]
  messages (SamePuzzleSet p) = [Reply $ mconcat ["Nian är redan satt till ", showt p]]
  messages (InvalidPuzzleSet p) = [Reply $ mconcat ["Pusslet " <> showt p <> " är inte giltigt!"]]
