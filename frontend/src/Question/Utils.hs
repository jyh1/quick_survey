{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts    #-}

module Question.Utils where

import Reflex.Dom hiding (Value)
import qualified Data.Text as T
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map as Map


import FrontendCommon
    

getColWidth :: Int -> T.Text
getColWidth n
        | n > 0 = "width: " <> tshow (quot 100 n) <> "%;"
        | otherwise = ""

{-# INLINABLE mutexDyn #-}
mutexDyn :: (MonadWidget t m) => Event t () -> Event t () -> m (Dynamic t Int)
mutexDyn p m = 
    foldDyn (+) 0 
                (mergeWith (+) [ 1 <$ p, -1 <$ m])

semRadioGroup :: (MonadWidget t m, Eq a)
           => Dynamic t T.Text
              -- ^ The name for the button group (different groups should be given different names)
           -> Dynamic t [(a,T.Text)]
              -- ^ Selectable values and their string labels
           -> Int
              -- ^ Number of columns
           -> GWidget t m (Maybe a)
              -- ^ Radio group in a 'GWidget' interface (function from 'WidgetConfig' to 'HtmlWidget' )
semRadioGroup dynName dynEntryList colN cfg = do
  let btns = (\pairs ->
        Map.fromList (zip [1..] (map fst pairs))) <$> dynEntryList

  buttonGroup "div" handleOne btns cfg

  where

    filedStyle = getColWidth colN
    mkBtnAttrs nm chked =
        "type" =: "checkbox"
     <> "name" =: nm
     <> "class" =: "hidden"
     <> if chked then "checked" =: "checked" else mempty
    mkCheckClass = "class" =: "ui checkbox radio"
    handleOne _ dynV dynChecked = do

      elAttr "div" ("class" =: "field radio-group-options" <> "style" =: filedStyle) $ elAttr "div" mkCheckClass $ do
        let txt = zipDynWith (\v m -> fromMaybe "" $ Prelude.lookup v m)
                             dynV dynEntryList

            btnAttrs = mkBtnAttrs <$> dynName <*> dynChecked
        _ <- elDynAttr' "input" btnAttrs $ return ()
        (b,_) <- el' "label" $ dynText txt
        f <- holdDyn False $ leftmost [ False <$ (Blur  `domEvent` b)
          , True  <$ (Focus `domEvent` b)]

        return (Click `domEvent` b, f)


displayAnswer :: (MonadWidget t m) => [T.Text] -> Dynamic t (Maybe Int) -> Dynamic t Int -> m ()
displayAnswer opts sel busy =
  elDynAttr "div" (zipDynWith selAttr sel busy) $ do
    encloseEle (> 0) $ do
      elAttr "i" ("class" =: "sync icon") blank
      text "Syncing"
    encloseEle (== 0) $ do
      elAttr "i" ("class" =: "check icon") blank
      dynText (showOpt <$> sel)
    return ()
    where 
      showOpt = maybe "None" (opts !!)
      visible p = "style" =: ("visibility: " <> maybe "hidden" (const "visible") p)
      display p = "style" =: ("display:" <> if p then "inline" else "none")
      encloseEle p ele = elDynAttr "div" ((display . p) <$> busy) ele
      selAttr p bc = ("class" =: ("ui left pointing basic label " <> if bc == 0 then "green" else "")) <> if bc == 0 then visible p else mempty


{-# INLINABLE textInputField #-}
textInputField :: MonadWidget t m => Maybe T.Text -> Maybe T.Text -> m (Dynamic t (Maybe Int), Event t T.Text)
textInputField placeholder initial = do
    textIn <- textInput (def{_textInputConfig_attributes = inputAttribute, _textInputConfig_initialValue = fromMaybe "" initial})
    return (trimInput <$> (value textIn), _textInput_input textIn)
    where
        inputAttribute = constDyn (("spellcheck" =: "false") <> ("placeholder" =: fromMaybe "" placeholder))
        trimInput b = 
          if T.null b then Nothing else Just 0