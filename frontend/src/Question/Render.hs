{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts    #-}

module Question.Render where

import Reflex.Dom hiding (Value)
import qualified Data.Text as T
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.IntMap.Strict as IM
import           Reflex.Dom.Contrib.Widgets.Common


import Common
import FrontendCommon
import Question.Utils

renderQuestionLis :: (MonadWidget t m) => Event t (PostRes t m, Form, SavedRes) -> m ()
renderQuestionLis upstreamE = do
  widgetHold (return never) (renderSurvey <$> upstreamE)
  return ()

renderSurvey :: (MonadWidget t m) => (PostRes t m, Form, SavedRes) -> m (Event t SurveyUpdate)
renderSurvey (postRes, qLis, saved) = divClass "ui form" $
  evalStateT (renderForm qLis) (FormState postRes 0 saved)

bumpCounter :: Monad m => RenderElement t m ()
bumpCounter = modify bump
  where bump (FormState x c s) = FormState x (c + 1) s

renderForm :: (MonadWidget t m) => Form -> RenderForm t m
renderForm form = do
  bumpCounter
  renderElement form


renderElement :: (MonadWidget t m) => Form -> RenderForm t m
renderElement (List elis) = do
  fs <- get
  (newState, response) <- lift $ do
    (es, newFs) <- divClass "ui segments" $ runStateT (mapM renderForm elis) fs
    return (newFs, leftmost es)
  put newState
  return response  
renderElement atomic = do
  FormState postRes count savedMap <- get
  lift (elAttr "div" segmentStyle (renderElementWith postRes count (IM.lookup count savedMap) atomic))
  where
    segmentStyle = "class" =: "ui segment" <> "style" =: "border-top: none;"


renderElementWith :: (MonadWidget t m) => PostRes t m -> Int -> Maybe ElementResponse -> Form -> m (Event t SurveyUpdate)
renderElementWith _ _ _ (Title title) = do
  divClass "ui dividing header" (text title)
  return never
renderElementWith _ _ _ (Plain t) = do
  el "p" (text t)
  return never
renderElementWith postRes rId saved (RadioGroup radioT radioO colCount) = do
  rec divClass "field" $ el "label" $ do
        text radioT
        displayAnswer radioO savedDyn busy
      answer <- optionRadioGroup (constDyn radioID) (constDyn radioO) (fromMaybe 0 colCount) savedId
      eventSel <- tailE (_hwidget_change answer)
      let eventResponse = getResponse <$> eventSel
      postToServer <- postRes rId eventResponse
      busy <- mutexDyn (() <$ eventResponse) (() <$ postToServer)
      savedDyn <- holdDyn savedId (fromResponse <$> postToServer)
  return ((\x -> (rId, x)) <$> eventResponse)
  where
    radioID = ("radio_" <> ) . tshow $ rId
    getResponse Nothing = Clear
    getResponse (Just k) = Clicked k
    fromResponse (Clicked k) = Just k
    fromResponse _ = Nothing
    savedId = saved >>= fromResponse

renderElementWith postRes rId saved (PlainText ptitle holder) = do
  rec divClass "field" $ el "label" $ do
        text ptitle
        displayAnswer ["Saved"] sel busy
      (sel, inputE) <- textInputField holder (saved >>= fromResponse)
      clearInputE <- delay (waitingTime + 0.1) inputE
      saveToServer <- debounce waitingTime inputE
      postToServer <- postRes rId (InputText <$> saveToServer)
      uploadingbusy <- mutexDyn (() <$ saveToServer) (() <$ postToServer)
      inputbusy <- mutexDyn (() <$ inputE) (() <$ clearInputE)
      let busy = zipDynWith mergeMutex inputbusy uploadingbusy
  return never
  where
    waitingTime = 1
    mergeMutex a b
      | a == 0 && b == 0 = 0
      | otherwise = 1
    fromResponse (InputText k) = Just k
    fromResponse _ = Nothing


optionRadioGroup :: MonadWidget t m => Dynamic t T.Text -> Dynamic t [T.Text] -> Int -> Maybe Int -> m (HtmlWidget t (Maybe Int))
optionRadioGroup groupK opts colN savedId =
    semRadioGroup 
          groupK
          (fmap (zip [0..]) opts) colN
          WidgetConfig { _widgetConfig_initialValue = savedId
                      , _widgetConfig_setValue     = never
                      , _widgetConfig_attributes   = constDyn ("class" =: "grouped fields")}


