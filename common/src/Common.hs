{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Text        (Text)
import           qualified Data.Text as T


import Types



sampleSurvey :: SurveyContent
sampleSurvey = 
  [
      Question {content = "What is your favorite color?", options = ["Blue","Red","Black"]}
    , Question {content = "What is your name?", options = ["No","Yes","Ok"]}
    , Question {content = "What is the readability?", options = ["1","2","3"]} 
  ]


tshow :: (Show a) => a -> T.Text
tshow = T.pack . show