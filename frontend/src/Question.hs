{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts    #-}

module Question where

import Reflex.Dom hiding (Value)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding(encodeUtf8)
import Data.Text.Lazy(fromStrict)
import Data.Aeson 
import Data.Aeson.Types (Parser, typeMismatch, parseMaybe)
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import Control.Monad.State

import Data.ByteString.Lazy(ByteString)

import Data.Vector (toList)

import Common
import FrontendCommon

jsonToQuestion :: T.Text -> Maybe Form
jsonToQuestion = parseSurvey . textToSurveyContent

textToSurveyContent :: T.Text -> ByteString
textToSurveyContent = encodeUtf8 . fromStrict
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


parseSurvey :: SurveyContent -> Maybe Form
parseSurvey raw = (decode raw) >>= parseMaybe parseForm 
-- parseSurvey _ = List [
--   Title "Question 1",
--   RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"],
--   List [Plain "Philip Thomas will be an assistant professor at the University of Massachusetts Amherst starting in September. Before that,", RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"], RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"]],
--   Title "Question 2",
--   RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"]
--   , RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"]
--   ]
parseSurvey' :: SurveyContent -> Form
parseSurvey' raw = fromMaybe (List []) (parseSurvey raw)

renderQuestionLis :: (MonadWidget t m) => Event t (PostRes t m, Form) -> m ()
renderQuestionLis upstreamE = do
  widgetHold (return never) (uncurry renderSurvey <$> upstreamE)
  return ()

renderSurvey :: (MonadWidget t m) => PostRes t m -> Form -> m (Event t SurveyUpdate)
renderSurvey postRes qLis = divClass "ui form" $
  evalStateT (renderForm qLis) (FormState postRes 0)

bumpCounter :: Monad m => RenderElement t m ()
bumpCounter = modify bump
  where bump (FormState x c) = FormState x (c + 1)

renderForm :: (MonadWidget t m) => Form -> RenderForm t m
renderForm form = do
  bumpCounter
  renderElement form
  -- elementRes <- mapM (renderElement postRes) elis
  -- return (leftmost elementRes)
-- renderForm = undefined

-- renderSingle :: (MonadWidget t m) => PostRes t m -> ElementWithID -> m (Event t SurveyUpdate)
-- renderSingle postRes ele = divClass "ui form" (renderElement postRes ele)

renderElement :: (MonadWidget t m) => Form -> RenderForm t m
renderElement (List elis) = do
  fs <- get
  (newState, response) <- lift $ do
    (es, newFs) <- divClass "ui segments" $ runStateT (mapM renderForm elis) fs
    return (newFs, leftmost es)
  put newState
  return response  
renderElement atomic = do
  FormState postRes count <- get
  lift (elAttr "div" segmentStyle (renderElementWith postRes count atomic))
  where
    segmentStyle = "class" =: "ui segment" <> "style" =: "border-top: none;"
    -- segmentStyle = "class" =: "ui segment"
-- renderElement (RadioGroup radioT radioO)
-- renderElement _ (_, Title title) = divClass "ui segment" $ divClass "ui segments" $ do
  -- text title
  -- return never

renderElementWith :: (MonadWidget t m) => PostRes t m -> Int -> Form -> m (Event t SurveyUpdate)
renderElementWith _ _ (Title title) = do
  divClass "ui dividing header" (text title)
  return never
renderElementWith _ _ (Plain t) = do
  el "p" (text t)
  return never
renderElementWith postRes rId (RadioGroup radioT radioO colCount) = do
  rec divClass "field" $ el "label" $ do
        text radioT
        displayAnswer radioO savedDyn busy
      answer <- optionRadioGroup (constDyn radioID) (constDyn radioO) (fromMaybe 0 colCount)
      eventSel <- updated <$> holdUniqDyn (_hwidget_value answer)
      let eventResponse = getResponse <$> eventSel
      postToServer <- postRes rId eventResponse
      -- TODO Initial value of displayAnswer
      busy <- foldDyn (+) 0 
                (mergeWith (+) [ 1 <$ eventResponse, -1 <$ postToServer])
      savedDyn <- holdDyn Nothing (fromResponse <$> postToServer)
  return ((\x -> (rId, x)) <$> eventResponse)
  where
    radioID = ("radio_" <> ) . tshow $ rId
    getResponse Nothing = Clear
    getResponse (Just k) = Clicked k
    fromResponse Clear = Nothing
    fromResponse (Clicked k) = Just k
    

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

optionRadioGroup :: MonadWidget t m => Dynamic t T.Text -> Dynamic t [T.Text] -> Int -> m (HtmlWidget t (Maybe Int))
optionRadioGroup groupK opts colN =
  -- rbs :: HtmlWidget t (Maybe Int) <- 
    semRadioGroup 
          groupK
          (fmap (zip [0..]) opts) colN
          WidgetConfig { _widgetConfig_initialValue = Nothing
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

    filedStyle = 
      "display: inline-block; padding-left: .5em; padding-right: .5em;" <> getColWidth colN
    mkBtnAttrs nm chked =
        "type" =: "checkbox"
     <> "name" =: nm
     <> "class" =: "hidden"
     <> if chked then "checked" =: "checked" else mempty
    -- mkCheckClass chked = "class" =: ("ui checkbox radio")
    mkCheckClass = "class" =: "ui checkbox radio"
    handleOne _ dynV dynChecked = do

      elAttr "div" ("class" =: "field" <> "style" =: filedStyle) $ elAttr "div" mkCheckClass $ do
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