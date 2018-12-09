{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module CreateSurvey(createOrFetch) where

import Reflex.Dom.Core
import qualified Data.Text as T
import           Data.Monoid ((<>))
import Data.Maybe (isJust)
import Control.Monad (join)
import qualified Data.IntMap.Strict as IM

import Question
import Fileinput
import Request
import FrontendCommon

type SurveyError t = Event t String

fileInputButton :: MonadWidget t m => m (Event t T.Text)
fileInputButton = divClass "field" $
  elClass "label" "ui orange button" $ do
    text "Create"
    fileButton <- fileInput (def & attributes .~ (constDyn ("style"=:"display:none")))
    getFileEvent fileButton
createSurvey :: MonadWidget t m => m (FetchSurvey t m, SurveyError t)
createSurvey = do
  divClassId "ui icon header" "file-icon-title" $ do
    elClassId "i" "file alternate outline icon" "file-icon" blank
    text "Add a Survey"
  uploadContent <- fileInputButton
  let fetchEvent = parseWithOriginal <$> uploadContent
  return (filterRight fetchEvent, filterLeft fetchEvent)
    where
      dummyPost _ res = return res
      parseWithOriginal y = 
        let ybyte = textToSurveyContent y in
          (\x -> SurveyCreation dummyPost x ybyte) <$> parseSurvey ybyte



inputField :: MonadWidget t m => 
  T.Text -> T.Text -> Dynamic t Bool -> m (Dynamic t (Maybe T.Text), Event t (), Event t ())
inputField label icon errorStatus = elDynClass "div" (fieldError <$> errorStatus) $ do
  elClass "label" "find-survey" (text label)
  divClass "ui left icon input find-survey" $ do
    (searchNameValue, onType, searchName) <- surveyNameInput label
    (e, _) <- elClass' "i" (icon <> " icon") blank
    let performSearch = keypress Enter searchName
    return (searchNameValue, onType, performSearch)
  where
    fieldError b = "field" <> if b then " error" else ""

surveyName, userName :: MonadWidget t m => Dynamic t Bool -> m (Dynamic t (Maybe T.Text), Event t (), Event t ())  
surveyName = inputField "Survey Name" "search"
userName = inputField "Your Name" "user"
searchButton :: MonadWidget t m => Dynamic t Bool -> m (Event t ())  
searchButton active = divClass "field" $ do
  (e, _) <- elDynClass' "div" (dynDisable <$> active) $ do
    text "Search"
  return (domEvent Click e)
  where 
    dynDisable b = "ui orange submit button" <> if b then "" else " disabled"

-- User and Survey
searchInfo :: MonadWidget t m => Dynamic t Bool -> m (Event t (), Event t (T.Text, T.Text))
searchInfo errorStatus = do
  (searchNameValue, onSurveyInput, onSurveyEnter) <- surveyName errorStatus
  (userNameValue, onUserInput, onUserEnter) <- userName (constDyn False)
  let surveyAndUser = zipDynWith mergeInput searchNameValue userNameValue
  submit <- searchButton (isJust <$> surveyAndUser)
  let onSearch = leftmost [onUserEnter, onSurveyEnter, submit]
      onInput = onSurveyInput
  return (onInput, tagMaybe (current surveyAndUser) onSearch)
  where
    mergeInput justA justB = do
      a <- justA
      b <- justB
      return (a, b)


searchSurvey :: MonadWidget t m => m (FetchSurvey t m, Dynamic t Bool, Dynamic t T.Text)
searchSurvey = do
      rec
        (onInput, onSearch) <- searchInfo errorStatus
        let surveyName = fst <$> onSearch
        dynSurveyName <- holdDyn "" surveyName
        let (fetch, postResponse, _) = ajaxFunctions (Right <$> dynSurveyName)
        (successRaw, fail) <- fetch (() <$ onSearch)
        let success = filterRight (parseSurvey <$> successRaw)
        let clearError = leftmost [() <$ success, onInput]
        errorStatus <- foldDyn const False 
          (mergeWith (&&) [True <$ fail, False <$ clearError])
      currentUser <- holdDyn (Left "None") ((Right . snd) <$> onSearch)
      -- request saved responses
      parsedFormB <- current <$> (holdDyn emptyForm success)
      saved <- getResponse (Right <$> dynSurveyName) currentUser (() <$ success)
      let savedMap = IM.fromList <$> saved
      let name_form_saved = attach (current dynSurveyName) (attach parsedFormB savedMap)

      errorMessage <- holdDyn "None" fail
      return (
          (\(n, (f, s)) -> 
            SurveySearch (postResponse currentUser) f n s) <$> name_form_saved, 
          errorStatus, errorMessage)

findSurvey :: MonadWidget t m => m (FetchSurvey t m)
findSurvey = divClass "ui form" $ do
  rec
    divClass "field" $ elClassId "h3" "ui header" "search-icon" $ do
      elDynClass "i" (icon <$> errorStatus) blank
      divClass "content" (dynText (join ((message errMsg)<$> errorStatus)))
    (fetched, errorStatus, errMsg) <- searchSurvey
  return fetched
  where
    message _ False = constDyn "Fill a Survey"
    message err True = err
    icon True = "exclamation circle icon"
    icon False = "edit alternate icon"

parsingError :: MonadWidget t m => Event t T.Text -> Event t () -> m ()
parsingError msg clear = do
  isHidden <- holdDyn "hidden" (leftmost ["" <$ msg, "hidden" <$ clear])
  let msgAttr = ("ui negative tiny message " <>) <$> isHidden
  elDynClass "div" msgAttr $ do
    divClass "header" (text "Parsing Error")
    errMsg <- holdDyn "" msg
    elClass "ul" "list" $ el "li" (dynText errMsg)

homePanel :: MonadWidget t m => m (FetchSurvey t m, SurveyError t)
homePanel = divClassId "ui placeholder segment" "survey-entry" $ 
  divClass "ui two column very relaxed stackable grid" $ do
    divClass "ui vertical divider" (text "Or")
    divClass "middle aligned row" $ do
      (loadedSurvey, errE) <- divClass "ui column stackable center aligned" createSurvey
      surveyNameSearch <- divClass "column" $ divClass "field" findSurvey
      return (leftmost [loadedSurvey, surveyNameSearch], errE)

createOrFetch :: MonadWidget t m => m (FetchSurvey t m)
createOrFetch = do
  rec
    parsingError ((T.pack . drop 12) <$> errE) (() <$ fetchEvent)
    (fetchEvent, errE) <- homePanel
  return fetchEvent
