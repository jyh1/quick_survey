{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Monoid ((<>))


import Question
import Fileinput
import Debug (run)
import Types


import Request
import Utilities

main :: IO ()
main = run 3003 $ mainWidgetWithHead headElement $ divClass "ui container" $ do
  createE <- createOrFetch
  -- inputConfig <- loadingFile
  -- let qLisE = fmapMaybe jsonToQuestion inputConfig
  -- let parsedQs = parseSurvey <$> qLisE
  -- (surveyName, submitE) <- inputSurveyName
  -- let (fetchSurvey, postRes, saveSurvey) = ajaxFunctions searchTag
  -- saveSurvey qLisE submitE
  -- (testSurveys, _) <- fetchSurvey (() <$ nameSearched)
  -- let qLis =  parseSurvey <$> leftmost [
                  -- (parseSurvey testQuestion) <$ buildE
                  -- parseSurvey <$> testSurveys

              -- ]
  renderQuestionLis createE
  -- response <- renderQuestionLis ((postAnswer searchResult) (constDyn "jyh1")) qLis
  -- responseHistory <- (foldDyn (:) [] response)
  -- display responseHistory

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Survey"
  styleSheet "static/semantic.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

inputSurveyName :: MonadWidget t m => m (Dynamic t T.Text, Event t ())
inputSurveyName = do
  name <- divClass "ui input" (_textInput_value <$> textInput def)
  submitBtn <- button "submit"
  return (name, submitBtn)


type FetchSurvey t m = Event t (PostRes t m, SurveyContent)


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
      dummyPost _ res = delay 0.1 res

  
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
      clickable "" = "circular search icon"
      clickable _ = "circular search icon link"
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


createOrFetch :: MonadWidget t m => m (FetchSurvey t m)
createOrFetch = divClass "ui placeholder segment" $ 
  divClass "ui two column stackable center aligned grid" $ do
    divClass "ui vertical divider" (text "Or")
    divClass "middle aligned row" $ do
      loadedSurvey <- divClass "column" createSurvey
      surveyNameSearch <- divClass "column" findSurvey
      return (leftmost [loadedSurvey, surveyNameSearch])
