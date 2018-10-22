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
import           Data.Bool                  (bool)
import           Data.Maybe                 (fromMaybe, listToMaybe)
-- import           JSDOM.Generated.HTMLInputElement (setChecked, HTMLInputElement (..))
import JSDOM.Types (castTo, Element)
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import Data.Traversable (mapAccumR)

import Datatype

jsonToQuestion :: T.Text -> Maybe [Question]
jsonToQuestion = decode' . encodeUtf8 . fromStrict

-- return next available id and parsed question
parseQuestion :: ElementID -> Question -> (ElementID, ParsedQuestion)
parseQuestion eid que = 
  (succ $ succ eid,
    [(eid, Title ("Question_" <> tshow eid)), (succ eid, RadioGroup (Just (content que)) (options que)) ]
  )

parseSurvey :: [Question] -> Survey
parseSurvey qlis =  snd (mapAccumR parseQuestion 0 qlis)

renderQuestionLis :: (MonadWidget t m) => Event t Survey -> m (Event t (Maybe Int))
renderQuestionLis qLis =
  let qIDs = map tshow [1..]
      qMap = Map.fromList . zip qIDs <$> qLis 
  in
    do
      -- surveyMap :: Dynamic t (Map k (Event t (Maybe Int)))
      -- surveyMap <- divClass "ui bottom attached segment form" $ listWithKey qMap renderQuestion
      widgetHold (return [never]) (mapM renderQuestion <$> qLis)
      -- let 
        -- surveyEvent :: Dynamic t (Event t (Maybe Int))
        -- surveyEvent = (leftmost . Map.elems) <$> surveyMap
      -- return (switchDyn surveyEvent)
      return never



renderQuestion :: (MonadWidget t m) => ParsedQuestion -> m (Event t ())
renderQuestion elis = do
  -- simpleList elis renderDynEle
  mapM renderElement elis
  return never

-- renderDynEle :: (MonadWidget t m) => Dynamic t ElementWithID -> m (Event t ElementResponse)
-- renderDynEle dynEle = do
--   display dynEle
--   switchDyn <$> widgetHold (return never) (renderElement <$> (traceEvent "dynele" (updated dynEle)))

renderElement :: (MonadWidget t m) => ElementWithID -> m (Event t ElementResponse)
renderElement (_, Title title) = divClass "ui top attached segment" $ do
  text title
  return never

renderElement (rId, RadioGroup radioT radioO) = divClass "ui bottom attached segment field" $ do
  rec el "label" $ do
        text (maybe "" id radioT)
        displayAnswer (constDyn radioO) (_hwidget_value answer)
      answer <- optionRadioGroup (constDyn radioID) (constDyn radioO)
  return (getResponse <$> _hwidget_change answer)
  where
    -- radio = getEle radioWithID
    radioID = ("radio_" <> ) . tshow $ rId
    getResponse Nothing = Clear
    getResponse (Just k) = Clicked k

displayAnswer :: (MonadWidget t m) => Dynamic t [T.Text] -> Dynamic t (Maybe Int) -> m ()
displayAnswer opts sel = elDynAttr "div" (selAttr <$> sel) $
  dynText (zipDynWith showOpt opts sel)
    where 
      showOpt opts Nothing = "None"
      showOpt opts (Just k) = opts !! k
      visible p = "style" =: ("display: " <> if (p == Nothing) then "none" else "inline")
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

    mkBtnAttrs nm =
        "type" =: "radio"
     <> "name" =: nm
    mkCheckClass chked = "class" =: ("ui checkbox radio" <> if chked then " checked" else "")
    handleOne _ dynV dynChecked = do

      divClass "field" $ elDynAttr "div" (mkCheckClass <$> dynChecked) $ do
        let txt = zipDynWith (\v m -> fromMaybe "" $ Prelude.lookup v m)
                             dynV dynEntryList

            btnAttrs = mkBtnAttrs <$> dynName
            -- btnAttrs = mkBtnAttrs <$> dynName <*> dynChecked
        (b,_) <- elDynAttr' "input" btnAttrs $ return ()
        f <- holdDyn False $ leftmost [ False <$ (Blur  `domEvent` b)
                                      , True  <$ (Focus `domEvent` b)]
        el "label" $ dynText txt
--        let e = castToHTMLInputElement $ _element_raw b
        -- Just e <- castTo HTMLInputElement $ _element_raw b
        -- _ <- performEvent $ (setChecked e) <$> updated dynChecked
        return (Click `domEvent` b, f)



testQuestion :: [Question]
testQuestion = 
  [
      Question {content = "What is your favorite color?", options = ["Blue","Red","Black"]}
    , Question {content = "What is your name?", options = ["No","Yes","Ok"]}
    , Question {content = "What is the readability?", options = ["1","2","3"]} 
  ]