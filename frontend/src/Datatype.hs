{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Datatype where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Question = Question {
      content :: T.Text
    , options  :: [T.Text]
    } deriving (Generic, Show)

instance ToJSON Question where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Question

data QuestionElement = 
      RadioGroup {title :: Maybe T.Text, radioOpts :: [T.Text]}
    | Title T.Text
    deriving (Show, Eq)

type ParsedQuestion = [(ElementID, QuestionElement)]

data Response = Clicked Int
    deriving (Show, Eq)

type ElementID = Int

data SurveyUpdate = SurveyUpdate {field :: ElementID, response :: Response}