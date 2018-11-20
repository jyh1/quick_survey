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
import Control.Monad.State


import Common
import FrontendCommon

jsonToQuestion :: T.Text -> Maybe [Question]
jsonToQuestion = decode' . encodeUtf8 . fromStrict

-- return next available id and parsed question
-- parseQuestion :: ElementID -> Question -> (ElementID, ParsedQuestion)
-- parseQuestion eid que = 
--   (succ $ succ eid,
--     [(eid, Title ("Question_" <> tshow eid)), (succ eid, RadioGroup (Just (content que)) (options que)) ]
--   )

parseSurvey :: SurveyContent -> Form
-- parseSurvey qlis =  snd (mapAccumL parseQuestion 0 qlis)
parseSurvey _ = List [
  Title "Question 1",
  List [RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"], RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"]],
  Title "Question 2",
  RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"]
  , RadioGroup "What car are you dirving?" ["Ford", "Vauxhall", "Volkswagen"]
  ]


renderQuestionLis :: (MonadWidget t m) => Event t (PostRes t m, SurveyContent) -> m ()
renderQuestionLis upstreamE = do
  widgetHold (return never) (uncurry renderSurvey <$> upstreamE)
  return ()

renderSurvey :: (MonadWidget t m) => PostRes t m -> SurveyContent -> m (Event t SurveyUpdate)
renderSurvey postRes qLis = divClass "ui form" $
  evalStateT (renderForm (parseSurvey qLis)) (FormState postRes 0)

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
  lift (divClass "ui segment" (renderElementWith postRes count atomic))
-- renderElement (RadioGroup radioT radioO)
-- renderElement _ (_, Title title) = divClass "ui segment" $ divClass "ui segments" $ do
  -- text title
  -- return never

renderElementWith :: (MonadWidget t m) => PostRes t m -> Int -> Form -> m (Event t SurveyUpdate)
renderElementWith _ _ (Title title) = do
  divClass "ui header" (text title)
  return never
renderElementWith postRes rId (RadioGroup radioT radioO) = do
  rec el "label" $ do
        text radioT
        displayAnswer radioO savedDyn busy
      answer <- optionRadioGroup (constDyn radioID) (constDyn radioO)
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