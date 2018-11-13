{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module CreateSurvey(createOrFetch) where

import Reflex.Dom.Core
import qualified Data.Text as T
import           Data.Monoid ((<>))

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
  T.Text -> T.Text -> Dynamic t Bool -> m (Dynamic t T.Text, Event t (), Event t ())
inputField label icon errorStatus = elDynClass "div" (fieldError <$> errorStatus) $ do
  el "label" (text label)
  divClass "ui left icon input" $ do
    searchName <- textInput (def & attributes .~ inputAttribute)
    let searchNameValue = T.strip <$> value searchName 
    (e, _) <- elClass' "i" (icon <> " icon") blank
    let performSearch = keypress Enter searchName
    return (searchNameValue, () <$ _textInput_input searchName, performSearch)
  where
    inputAttribute = constDyn (("spellcheck" =: "false") <> ("placeholder" =: "Survey Name"))
    fieldError b = "field" <> if b then " error" else ""

surveyName, userName :: MonadWidget t m => Dynamic t Bool -> m (Dynamic t T.Text, Event t (), Event t ())  
surveyName = inputField "Survey Name" "search"
userName = inputField "Your Name" "user"
searchButton :: MonadWidget t m => Dynamic t Bool -> m (Event t ())  
searchButton active = divClass "field" $ do
  (e, _) <- elClass' "div" "ui blue submit button" $ do
    text "Search"
  return (domEvent Click e)
  

searchSurvey :: MonadWidget t m => m (FetchSurvey t m, Dynamic t Bool)
searchSurvey = do
      rec
        (searchNameValue, onInput, performSearch) <- surveyName errorStatus
        userName errorStatus
        searchButton errorStatus
        let (fetch, postResponse, _) = ajaxFunctions (eitherValue <$> searchNameValue)
        (success, fail) <- fetch performSearch
        let clearError = leftmost [() <$ success, onInput]
        errorStatus <- foldDyn const False 
          (mergeWith (&&) [True <$ fail, False <$ clearError])
      errorMessage <- holdDyn "None" fail
      return ((\x -> (postResponse (constDyn "jyh1"), x)) <$> success, errorStatus)
    where
      eitherValue "" = Left "None"
      eitherValue s = Right s
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
