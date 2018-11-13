{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module CreateSurvey(createOrFetch) where

import Reflex.Dom.Core
import qualified Data.Text as T
import           Data.Monoid ((<>))
import Data.Maybe (isJust)

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
  divClass "ui icon header" $ do
    elClass "i" "file alternate outline icon" blank
    text "Add a Survey"
  uploadContent <- fileInputButton
  return ((\x -> (dummyPost, x)) <$> (fmapMaybe jsonToQuestion uploadContent))
    where
      dummyPost _ res = return res



inputField :: MonadWidget t m => 
  T.Text -> T.Text -> Dynamic t Bool -> m (Dynamic t (Maybe T.Text), Event t (), Event t ())
inputField label icon errorStatus = elDynClass "div" (fieldError <$> errorStatus) $ do
  el "label" (text label)
  divClass "ui left icon input" $ do
    searchName <- textInput (def & attributes .~ inputAttribute)
    let searchNameValue = T.strip <$> value searchName 
    (e, _) <- elClass' "i" (icon <> " icon") blank
    let performSearch = keypress Enter searchName
    return (trimInput <$> searchNameValue, () <$ _textInput_input searchName, performSearch)
  where
    inputAttribute = constDyn (("spellcheck" =: "false") <> ("placeholder" =: label))
    fieldError b = "field" <> if b then " error" else ""
    trimInput b = 
      let trimed = T.strip b in
        if T.null trimed then Nothing else Just trimed

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

  
searchSurvey :: MonadWidget t m => m (FetchSurvey t m, Dynamic t Bool)
searchSurvey = do
      rec
        (onInput, onSearch) <- searchInfo errorStatus
        dynSurveyName <- holdDyn (Left "None") ((Right . fst) <$> onSearch)
        let (fetch, postResponse, _) = ajaxFunctions dynSurveyName
        (success, fail) <- fetch (() <$ onSearch)
        let clearError = leftmost [() <$ success, onInput]
        errorStatus <- foldDyn const False 
          (mergeWith (&&) [True <$ fail, False <$ clearError])
      currentUser <- holdDyn (Left "None") ((Right . snd) <$> onSearch)
      errorMessage <- holdDyn "None" fail
      return ((\x -> (postResponse currentUser, x)) <$> success, errorStatus)

findSurvey :: MonadWidget t m => m (FetchSurvey t m)
findSurvey = divClass "ui form" $ divClass "field" $ do
  rec
    elClass "h3" "ui header" $ do
      elDynClass "i" (icon <$> errorStatus) blank
      divClass "content" $ dynText (message <$> errorStatus)
    (fetched, errorStatus) <- searchSurvey
  return fetched
  where
    message False = "Fill a Survey"
    message True = "Survey Not Found"
    icon True = "exclamation circle icon"
    icon False = "pencil alternate icon"
-- fillSurvey :: MonadWidget t m => m (FetchSurvey t m)
-- fillSurvey = do
--   divClass "two fields" $ do
--     findSurvey
--     findSurvey
  

createOrFetch :: MonadWidget t m => Dynamic t Bool -> m (FetchSurvey t m)
createOrFetch hide = hideDynDivClass hide "ui placeholder segment" $ 
  divClass "ui two column very relaxed stackable grid" $ do
    divClass "ui vertical divider" (text "Or")
    divClass "middle aligned row" $ do
      loadedSurvey <- divClass "ui column stackable center aligned" createSurvey
      surveyNameSearch <- divClass "column" $ divClass "field" findSurvey
      return (leftmost [loadedSurvey, surveyNameSearch])