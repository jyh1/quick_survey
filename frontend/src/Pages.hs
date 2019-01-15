{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Pages where

import Reflex.Dom.Core hiding (Home, Submit, Reset)
import qualified Data.Text as T
import Data.Monoid ((<>))

import Question.Question
import Fileinput
import Debug (run)
import CreateSurvey
import Tabs
import FrontendCommon
import SurveyUpload (submitForm)
import Response (responseInfo)
import Header (headerElement)
import Example (exampleSurvey)


changeTab :: SurveyGeneration t m -> PageStatus
changeTab (SurveySearch {}) = PageStatus [Home, Survey, Responses] Survey
changeTab (SurveyCreation {}) = PageStatus [Home, Preview, Submit] Preview

homePage :: MonadWidget t m => Dynamic t Page -> m (Event t PageStatus, FetchSurvey t m)
homePage active = do
  createE <- displayPage active Home createOrFetch
  return (changeTab <$> createE, createE)


previewPage, surveyPage :: MonadWidget t m => Dynamic t Page -> FetchSurvey t m -> m ()
previewPage active createE = 
  displayPage active Preview $ do
    divClass "ui warning tiny message" $ do
      divClass "header" (text "Preview Only")
      elClass "ul" "list" $ el "li" (text "Your responses here will not be saved")

    renderQuestionLis (getRenderForm <$> createE)

surveyPage active createE =
  displayPage active Survey (renderQuestionLis (getRenderForm <$> createE))
  
examplePage :: MonadWidget t m => Dynamic t Page -> m ()
examplePage active =
  displayPage active Example $ do
    divClass "ui info message" $ do
      divClass "header" (text "An Example Survey")
      elClass "ul" "list" $ do
        el "li" $ do
          text "Upload this "
          elAttr "a" (("href" =: getSurveyContent exampleName) <> ("download" =: "sample.json")) (text "config file")
          text " to create the same survey as below."
        el "li" (text ("Or use survey name \"" <> exampleName <> "\" to search"))
        el "li" (text "All responses will be saved instantly.")
        el "li" (text "Progress will be resumed when entering the survey with the same user name.")
    
    ex <- exampleSurvey
    renderQuestionLis (getRenderForm <$> ex)

submitPage :: MonadWidget t m => Dynamic t Page -> Event t SurveyContent -> m ()
submitPage active surveyE = 
  displayPage active Submit (divClass "ui segment" $ submitForm surveyE)

responsePage :: MonadWidget t m => Dynamic t Page -> Event t T.Text -> m ()
responsePage active surveyIdE = 
  displayPage active Responses (divClass "ui segment" $ responseInfo surveyIdE)

allPages :: MonadWidget t m => m ()
allPages = do
  divClass "header-bar" headerElement
  rec
    curPage <- divClass "menu-background" $ do
      rec
        pageStatus <- foldDyn ($) initialPage (leftmost [navClick, surCreate])
        let curPage = activated <$> pageStatus
        navClick <- fmap (clickActive <$>) (breadCrumb pageStatus)
      return curPage

    surCreate <- divClassId "ui container" "main-body" $ do
      (newStatus, createE) <- homePage curPage
      examplePage curPage
      submitPage curPage (fmapMaybe getContent createE)
      previewPage curPage createE
      surveyPage curPage createE
      responsePage curPage (fmapMaybe getName createE)
      return (const <$> newStatus)

  return ()

initialPage :: PageStatus
initialPage = PageStatus [Home, Example] Home

clickActive :: Page -> PageStatus -> PageStatus
clickActive act (PageStatus ps _) = PageStatus ps act

displayPage :: MonadWidget t m => Dynamic t Page -> Page -> m a -> m a
displayPage focus current w =
  divDynClass tabClass w
  where
    getTab foc
      | foc == current = "ui active tab survey-tab"
      | otherwise = "ui tab survey-tab"
    tabClass = getTab <$> focus
