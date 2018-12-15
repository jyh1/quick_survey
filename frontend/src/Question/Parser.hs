{-# LANGUAGE OverloadedStrings #-}

module Question.Parser(parseSurvey, jsonToQuestion, textToSurveyContent) where

import FrontendCommon
import Common
import Data.Aeson 
import Data.Aeson.Types (Parser, typeMismatch, parseEither)
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8)
import           Data.Monoid ((<>))
import Data.Vector (toList)
import Data.ByteString.Lazy(fromStrict)


jsonToQuestion :: T.Text -> Either String Form
jsonToQuestion = parseSurvey . textToSurveyContent

textToSurveyContent :: T.Text -> SurveyContent
textToSurveyContent = encodeUtf8

parseRadioGroup, parseTitle, parseTextInput, parseFormObject :: Object -> Parser Form
parseRadioGroup obj = do
  title <- obj .: "title"
  choices <- obj .: "choices"
  colCount <- obj .:? "colCount"
  return (RadioGroup title choices colCount)

parseTitle obj = Title <$> (obj .: "title")

parseTextInput obj = do
  title <- obj .: "title"
  pl <- obj .:? "placeholder"
  return (PlainText title pl)

parseFormObject obj = do
  formType <- obj .:? "type"
  case formType of
    Nothing -> parseRadioGroup obj
    Just "radiogroup" -> parseRadioGroup obj
    Just "title" -> parseTitle obj
    Just "text" -> (obj .: "title") >>= parsePlain
    Just "textinput" -> parseTextInput obj
    Just other -> fail ("Unkonw type: " <> other)

parsePlain :: T.Text -> Parser Form
parsePlain t = return (Plain t)

parseForm :: Value -> Parser Form
parseForm (Object obj) = parseFormObject obj
parseForm (Array arr) = (List . toList) <$> mapM parseForm arr
parseForm (String str) = parsePlain str
parseForm val = typeMismatch "Form" val


parseSurvey :: SurveyContent -> Either String Form
parseSurvey raw = (eitherDecode' (fromStrict raw)) >>= parseEither parseForm 
