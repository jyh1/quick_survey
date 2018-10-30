{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Common where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Text        (Text)
import           qualified Data.Text as T
import           Servant.API


import Types

type QuestionAPI = "survey" :> Capture "surveyid" T.Text :> Get '[JSON] [Question]

sampleSurvey :: [Question]
sampleSurvey = 
  [
      Question {content = "What is your favorite color?", options = ["Blue","Red","Black"]}
    , Question {content = "What is your name?", options = ["No","Yes","Ok"]}
    , Question {content = "What is the readability?", options = ["1","2","3"]} 
  ]