{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T


import Question
import Fileinput
import Debug (run)


import Request

main :: IO ()
main = run 3003 $ mainWidgetWithHead headElement $ divClass "ui container" $ do
  createOrFetch
  inputConfig <- loadingFile
  let qLisE = fmapMaybe jsonToQuestion inputConfig
  let parsedQs = parseSurvey <$> qLisE
  (surveyName, submitE) <- inputSurveyName
  let (fetchSurvey, postRes, saveSurvey) = ajaxFunctions surveyName
  saveSurvey qLisE submitE
  (testSurveys, _) <- getPostBuild >>= fetchSurvey
  let qLis =  leftmost [
                  -- (parseSurvey testQuestion) <$ buildE
                  parseSurvey <$> testSurveys
                , parsedQs
              ]
  response <- renderQuestionLis (postRes (constDyn "jyh1")) qLis
  responseHistory <- (foldDyn (:) [] response)
  display responseHistory

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


fileInputButton :: MonadWidget t m => m ()
fileInputButton = do
  elClass "label" "ui primary button" $ do
    text "Create"
    fileInput (def & attributes .~ (constDyn ("style"=:"display:none")))
    return ()
createSurvey :: MonadWidget t m => m ()
createSurvey = do
  divClass "ui icon header" $ do
    elClass "i" "file alternate outline icon" blank
    text "Add New Survey"
  fileInputButton

  
searchSurvey :: MonadWidget t m => m ()
searchSurvey = divClass "field" $
    divClass "ui search" $
      divClass "ui icon input" $ do
        textInput def
        elClass "i" "search icon" blank
findSurvey :: MonadWidget t m => m ()
findSurvey = do
  divClass "ui icon header" $ do
    elClass "i" "search icon" blank
    text "Find Survey"
  searchSurvey


createOrFetch :: MonadWidget t m => m ()
createOrFetch = divClass "ui placeholder segment" $ 
  divClass "ui two column stackable center aligned grid" $ do
    divClass "ui vertical divider" (text "Or")
    divClass "middle aligned row" $ do
      divClass "column" createSurvey
      divClass "column" findSurvey