{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module Types where

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
      RadioGroup {radioTitle :: Maybe T.Text, radioOpts :: [T.Text]}
    | Title {titleTitle :: T.Text}
    deriving (Show, Eq)

type WithID key ele = (key, ele)

getId = fst
getEle = snd


type ElementWithID = WithID ElementID QuestionElement
type ParsedQuestion = [ElementWithID]

type Survey = [ParsedQuestion]

data ElementResponse = 
      Clicked ElementID
    | Clear
    deriving (Show, Eq)

type ElementID = Int

type FieldID = Int

type SurveyUpdate = WithID FieldID ElementResponse

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show