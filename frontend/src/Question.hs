{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Question where

import Reflex.Dom
import qualified Data.Text as T
import Data.Text.Lazy.Encoding(encodeUtf8)
import Data.Text.Lazy(fromStrict)
import Data.Aeson (decode')
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common

import Datatype

parseQuestion :: T.Text -> Maybe [Question]
parseQuestion = decode' . encodeUtf8 . fromStrict

renderQuestion :: (MonadWidget t m) => T.Text -> Dynamic t Question -> m (Dynamic t (Maybe Int))
renderQuestion groupK que = divClass "field" $ do
  rec el "label" $ do
        dynText (content <$> que)
        divClass "ui left pointing label" $ display answer
      answer <- divClass "inline fields" $
        optionRadioGroup groupK (options <$> que)
  return answer


optionRadioGroup :: MonadWidget t m => T.Text -> Dynamic t [T.Text] -> m (Dynamic t (Maybe Int))
optionRadioGroup groupK opts = divClass "inline fields" $ do
  rbs :: HtmlWidget t (Maybe Int) <- 
    radioGroup 
          (constDyn groupK)
          (fmap (zip [1..]) opts)
          WidgetConfig { _widgetConfig_initialValue = Nothing
                      , _widgetConfig_setValue     = never
                      , _widgetConfig_attributes   = constDyn mempty}
  return (_hwidget_value rbs)


testQuestion :: Question
testQuestion = Question {content = "What is your favorite color?", options = ["Blue","Red","Black"]}