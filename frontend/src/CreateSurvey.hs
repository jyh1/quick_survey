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
    text "Add New Survey"
  uploadContent <- fileInputButton
  return ((\x -> (dummyPost, x)) <$> (fmapMaybe jsonToQuestion uploadContent))
    where
      dummyPost _ res = return res

  
searchSurvey :: MonadWidget t m => m (FetchSurvey t m, Dynamic t Bool)
searchSurvey = divClass "field" $
    divClass "ui search" $ do
      rec
        (searchName, performSearch) <- elDynClass "div" (inputErrSty <$> errorStatus) $ do
          searchName <- textInput (def & attributes .~ inputAttribute)
          (e, _) <- elDynClass' "i" (clickable <$> searchNameValue) blank
          let performSearch = leftmost [domEvent Click e, keypress Enter searchName]
          return (searchName, performSearch)
        let searchNameValue = T.strip <$> value searchName
            (fetch, postResponse, _) = ajaxFunctions (eitherValue <$> searchNameValue)
        (success, fail) <- fetch performSearch
        let clearError = leftmost [() <$ success, () <$ _textInput_input searchName]
        errorStatus <- foldDyn const False 
          (mergeWith (&&) [True <$ fail, False <$ clearError])
      errorMessage <- holdDyn "None" fail
      return ((\x -> (postResponse (constDyn "jyh1"), x)) <$> success, errorStatus)
    where
      clickable "" = "inverted circular search icon"
      clickable _ = "inverted circular search icon link"
      eitherValue "" = Left "None"
      eitherValue s = Right s
      inputErrSty b = "ui icon input" <> if b then " error" else ""
      inputAttribute = constDyn (("spellcheck" =: "false") <> ("placeholder" =: "Survey Name"))
findSurvey :: MonadWidget t m => m (FetchSurvey t m)
findSurvey = do
  rec
    divClass "ui icon header" $ do
      elDynClass "i" (icon <$> errorStatus) blank
      divClass "ui red" $ dynText (message <$> errorStatus)
    (fetched, errorStatus) <- searchSurvey
  return fetched
  where
    message False = "Find Survey"
    message True = "Survey Not Found"
    icon True = "exclamation circle icon"
    icon False = "search icon"


createOrFetch :: MonadWidget t m => Dynamic t Bool -> m (FetchSurvey t m)
createOrFetch hide = hideDynDivClass hide "ui placeholder segment" $ 
  divClass "ui two column stackable center aligned grid" $ do
    divClass "ui vertical divider" (text "Or")
    divClass "middle aligned row" $ do
      loadedSurvey <- divClass "column" createSurvey
      surveyNameSearch <- divClass "column" findSurvey
      return (leftmost [loadedSurvey, surveyNameSearch])
