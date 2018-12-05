{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module CreateSurvey(createOrFetch) where

import Reflex.Dom.Core
import qualified Data.Text as T
import           Data.Monoid ((<>))
import Data.Maybe (isJust)
import Control.Monad (join)

import Question
import Fileinput
import Request
import FrontendCommon

fileInputButton :: MonadWidget t m => m (Event t T.Text)
fileInputButton =
  elClass "label" "ui primary button" $ do
    text "Create"
    fileButton <- fileInput (def & attributes .~ (constDyn ("style"=:"display:none")))
    getFileEvent fileButton
createSurvey :: MonadWidget t m => m (FetchSurvey t m)
createSurvey = do
  rec
    divClass "ui icon header" $ do
      elDynClass "i" fileIcon blank
      text "Add a Survey"
    uploadContent <- fileInputButton
    fileIcon <- foldDyn const "file outline icon" ("file alternate outline icon" <$ uploadContent)
  return (filterRight (parseWithOriginal <$> uploadContent))
    where
      dummyPost _ res = return res
      parseWithOriginal y = 
        let ybyte = textToSurveyContent y in
          (\x -> SurveyCreation dummyPost x ybyte) <$> parseSurvey ybyte



inputField :: MonadWidget t m => 
  T.Text -> T.Text -> Dynamic t Bool -> m (Dynamic t (Maybe T.Text), Event t (), Event t ())
inputField label icon errorStatus = elDynClass "div" (fieldError <$> errorStatus) $ do
  el "label" (text label)
  divClass "ui left icon input" $ do
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
    dynDisable b = "ui blue submit button" <> if b then "" else " disabled"

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
        dynSurveyName <- holdDyn (Left "None") ((Right . fst) <$> onSearch)
        let (fetch, postResponse, _) = ajaxFunctions dynSurveyName
        (successRaw, fail) <- fetch (() <$ onSearch)
        let success = filterRight (parseSurvey <$> successRaw)
        let clearError = leftmost [() <$ success, onInput]
        errorStatus <- foldDyn const False 
          (mergeWith (&&) [True <$ fail, False <$ clearError])
      currentUser <- holdDyn (Left "None") ((Right . snd) <$> onSearch)
      errorMessage <- holdDyn "None" fail
      return ((\x -> SurveySearch (postResponse currentUser) x) <$> success, errorStatus, errorMessage)

findSurvey :: MonadWidget t m => m (FetchSurvey t m)
findSurvey = divClass "ui form" $ divClass "field" $ do
  rec
    elClass "h3" "ui header" $ do
      elDynClass "i" (icon <$> errorStatus) blank
      divClass "content" (dynText (join ((message errMsg)<$> errorStatus)))
    (fetched, errorStatus, errMsg) <- searchSurvey
  return fetched
  where
    message _ False = constDyn "Fill a Survey"
    message err True = err
    icon True = "exclamation circle icon"
    icon False = "edit alternate icon"
  

createOrFetch :: MonadWidget t m => m (FetchSurvey t m)
createOrFetch = divClass "ui placeholder segment" $ 
  divClass "ui two column very relaxed stackable grid" $ do
    divClass "ui vertical divider" (text "Or")
    divClass "middle aligned row" $ do
      loadedSurvey <- divClass "ui column stackable center aligned" createSurvey
      surveyNameSearch <- divClass "column" $ divClass "field" findSurvey
      return (leftmost [loadedSurvey, surveyNameSearch])
