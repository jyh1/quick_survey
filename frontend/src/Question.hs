{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts    #-}

module Question where

import Reflex.Dom hiding (Value)
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8)
-- import Data.Text.Lazy(fromStrict)
import Data.ByteString.Lazy(fromStrict)
import Data.Aeson 
import Data.Aeson.Types (Parser, typeMismatch, parseEither)
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.IntMap.Strict as IM


-- import Data.ByteString.Lazy(ByteString)

import Data.Vector (toList)

import Common
import FrontendCommon

jsonToQuestion :: T.Text -> Either String Form
jsonToQuestion = parseSurvey . textToSurveyContent

textToSurveyContent :: T.Text -> SurveyContent
textToSurveyContent = encodeUtf8
-- return next available id and parsed question
-- parseQuestion :: ElementID -> Question -> (ElementID, ParsedQuestion)
-- parseQuestion eid que = 
--   (succ $ succ eid,
--     [(eid, Title ("Question_" <> tshow eid)), (succ eid, RadioGroup (Just (content que)) (options que)) ]
--   )

parseRadioGroup, parseTitle, parseFormObject :: Object -> Parser Form
parseRadioGroup obj = do
  title <- obj .: "title"
  choices <- obj .: "choices"
  colCount <- obj .:? "colCount"
  return (RadioGroup title choices colCount)

parseTitle obj = Title <$> (obj .: "title")

parseFormObject obj = do
  formType <- obj .:? "type"
  case formType of
    Nothing -> parseRadioGroup obj
    Just "radiogroup" -> parseRadioGroup obj
    Just "title" -> parseTitle obj
    Just "text" -> (obj .: "title") >>= parsePlain
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
      -- eventSel <- updated <$> holdUniqDyn (_hwidget_value answer)
      eventSel <- tailE (_hwidget_change answer)
      let eventResponse = getResponse <$> eventSel
      postToServer <- postRes rId eventResponse
      -- TODO Initial value of displayAnswer
      busy <- foldDyn (+) 0 
                (mergeWith (+) [ 1 <$ eventResponse, -1 <$ postToServer])
      savedDyn <- holdDyn savedId (fromResponse <$> postToServer)
  return ((\x -> (rId, x)) <$> eventResponse)
  where
    radioID = ("radio_" <> ) . tshow $ rId
    getResponse Nothing = Clear
    getResponse (Just k) = Clicked k
    fromResponse Clear = Nothing
    fromResponse (Clicked k) = Just k
    savedId = saved >>= fromResponse
    

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

optionRadioGroup :: MonadWidget t m => Dynamic t T.Text -> Dynamic t [T.Text] -> Int -> Maybe Int -> m (HtmlWidget t (Maybe Int))
optionRadioGroup groupK opts colN savedId =
  -- rbs :: HtmlWidget t (Maybe Int) <- 
    semRadioGroup 
          groupK
          (fmap (zip [0..]) opts) colN
          WidgetConfig { _widgetConfig_initialValue = savedId
                      , _widgetConfig_setValue     = never
                      , _widgetConfig_attributes   = constDyn ("class" =: "grouped fields")}
  -- return rbs


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
    -- mkCheckClass chked = "class" =: ("ui checkbox radio")
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

getColWidth :: Int -> T.Text
getColWidth n
        | n > 0 = "width: " <> tshow (quot 100 n) <> "%;"
        | otherwise = ""

-- testQuestion :: [Question]
-- testQuestion = sampleSurvey