{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts    #-}

module Question where

import Reflex.Dom
import qualified Data.Text as T
import Data.Text.Lazy.Encoding(encodeUtf8)
import Data.Text.Lazy(fromStrict)
import Data.Aeson (decode')
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import Data.Traversable (mapAccumL) 


import Types
import Common

jsonToQuestion :: T.Text -> Maybe [Question]
jsonToQuestion = decode' . encodeUtf8 . fromStrict

-- return next available id and parsed question
parseQuestion :: ElementID -> Question -> (ElementID, ParsedQuestion)
parseQuestion eid que = 
  (succ $ succ eid,
    [(eid, Title ("Question_" <> tshow eid)), (succ eid, RadioGroup (Just (content que)) (options que)) ]
  )

parseSurvey :: [Question] -> Survey
parseSurvey qlis =  snd (mapAccumL parseQuestion 0 qlis)

renderQuestionLis :: (MonadWidget t m) => Event t Survey -> m (Event t SurveyUpdate)
renderQuestionLis qLis = do
  allUpdates <- widgetHold (return [never]) (mapM renderQuestion <$> qLis)
  return (switchDyn (leftmost <$> allUpdates))


renderQuestion :: (MonadWidget t m) => ParsedQuestion -> m (Event t SurveyUpdate)
renderQuestion elis = do
  elementRes <- mapM renderElement elis
  return (leftmost elementRes)


renderElement :: (MonadWidget t m) => ElementWithID -> m (Event t SurveyUpdate)
renderElement (_, Title title) = divClass "ui top attached segment" $ do
  text title
  return never

renderElement (rId, RadioGroup radioT radioO) = divClass "ui bottom attached segment field form" $ do
  rec el "label" $ do
        text (maybe "" id radioT)
        displayAnswer (radioO) (_hwidget_value answer)
      answer <- optionRadioGroup (constDyn radioID) (constDyn radioO)
  dynVal <- holdUniqDyn (_hwidget_value answer)
  return (getResponse <$> (updated dynVal))
  where
    radioID = ("radio_" <> ) . tshow $ rId
    getResponse Nothing = (rId, Clear)
    getResponse (Just k) = (rId, Clicked k)

displayAnswer :: (MonadWidget t m) => [T.Text] -> Dynamic t (Maybe Int) -> m ()
displayAnswer opts sel = elDynAttr "div" (selAttr <$> sel) $
  dynText (showOpt <$> sel)
    where 
      showOpt = maybe "None" (opts !!)
      visible p = "style" =: ("visibility: " <> maybe "hidden" (const "visible") p)
      -- visible _ = "style" =: "inline"
      selAttr p = ("class" =: "ui left pointing label") <> visible p

optionRadioGroup :: MonadWidget t m => Dynamic t T.Text -> Dynamic t [T.Text] -> m (HtmlWidget t (Maybe Int))
optionRadioGroup groupK opts =
  -- rbs :: HtmlWidget t (Maybe Int) <- 
    semRadioGroup 
          groupK
          (fmap (zip [0..]) opts)
          WidgetConfig { _widgetConfig_initialValue = Nothing
                      , _widgetConfig_setValue     = never
                      , _widgetConfig_attributes   = constDyn ("class" =: "inline fields")}
  -- return rbs


semRadioGroup :: (MonadWidget t m, Eq a)
           => Dynamic t T.Text
              -- ^ The name for the button group (different groups should be given different names)
           -> Dynamic t [(a,T.Text)]
              -- ^ Selectable values and their string labels
           -> GWidget t m (Maybe a)
              -- ^ Radio group in a 'GWidget' interface (function from 'WidgetConfig' to 'HtmlWidget' )
semRadioGroup dynName dynEntryList cfg = do
  let btns = (\pairs ->
        Map.fromList (zip [1..] (map fst pairs))) <$> dynEntryList

  buttonGroup "div" handleOne btns cfg

  where

    mkBtnAttrs nm chked =
        "type" =: "checkbox"
     <> "name" =: nm
     <> "class" =: "hidden"
     <> if chked then "checked" =: "checked" else mempty
    -- mkCheckClass chked = "class" =: ("ui checkbox radio")
    mkCheckClass = "class" =: "ui checkbox radio"
    handleOne _ dynV dynChecked = do

      divClass "field" $ elAttr "div" mkCheckClass $ do
        let txt = zipDynWith (\v m -> fromMaybe "" $ Prelude.lookup v m)
                             dynV dynEntryList

            btnAttrs = mkBtnAttrs <$> dynName <*> dynChecked
        _ <- elDynAttr' "input" btnAttrs $ return ()
        (b,_) <- el' "label" $ dynText txt
        f <- holdDyn False $ leftmost [ False <$ (Blur  `domEvent` b)
          , True  <$ (Focus `domEvent` b)]

        return (Click `domEvent` b, f)



testQuestion :: [Question]
testQuestion = sampleSurvey