{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Datatype where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Data.Text.Lazy.Encoding
import Data.Text.Lazy as TL

data Question = Question {
      content :: T.Text
    , options  :: [T.Text]
    } deriving (Generic, Show)

instance ToJSON Question where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Question


testQuestion :: Question
testQuestion = Question {content = "What is your favorite color?", options = ["Blue","Red","Black"]}

parseQuestion :: T.Text -> Maybe [Question]
parseQuestion = decode' . encodeUtf8 . fromStrict