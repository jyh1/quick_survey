{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Monoid ((<>))


import Question
import Fileinput
import Debug (run)
import CreateSurvey


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
