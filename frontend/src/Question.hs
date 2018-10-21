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

import Datatype

parseQuestion :: T.Text -> Maybe [Question]
parseQuestion = decode' . encodeUtf8 . fromStrict

renderQuestion :: (MonadWidget t m) => T.Text -> Dynamic t Question -> m (Dynamic t (Maybe Int))
renderQuestion groupK que = divClass "field" $ do
  rec el "label" $ do
        dynText (content <$> que)
        displayAnswer (options <$> que) answer
      answer <- optionRadioGroup groupK (options <$> que)
  return answer

displayAnswer :: (MonadWidget t m) => Dynamic t [T.Text] -> Dynamic t (Maybe Int) -> m ()
displayAnswer opts sel = elDynAttr "div" (selAttr <$> sel) $
  dynText (zipDynWith showOpt opts sel)
    where 
      showOpt opts Nothing = "None"
      showOpt opts (Just k) = opts !! (k - 1)
      visible p = "style" =: ("display: " <> if (p == Nothing) then "none" else "inline")
      selAttr p = ("class" =: "ui left pointing label") <> visible p

optionRadioGroup :: MonadWidget t m => T.Text -> Dynamic t [T.Text] -> m (Dynamic t (Maybe Int))
optionRadioGroup groupK opts = do
  rbs :: HtmlWidget t (Maybe Int) <- 
    semRadioGroup 
          (constDyn groupK)
          (fmap (zip [1..]) opts)
          WidgetConfig { _widgetConfig_initialValue = Nothing
                      , _widgetConfig_setValue     = never
                      , _widgetConfig_attributes   = constDyn ("class" =: "inline fields")}
  return (_hwidget_value rbs)


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