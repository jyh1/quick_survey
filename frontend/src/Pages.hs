{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Pages where

import Reflex.Dom.Core hiding (Home, Submit, Reset)

import Question
import Fileinput
import Debug (run)
import CreateSurvey
import Tabs
import FrontendCommon
import SurveyUpload (submitForm)


homePage :: MonadWidget t m => Dynamic t Page -> m (Event t PageStatus, FetchSurvey t m)
homePage active = do
  createE <- displayPage active Home createOrFetch
  let newStat = (PageStatus [Home, Preview, Submit] Preview)
  return (newStat <$ createE, createE)


previewPage :: MonadWidget t m => Dynamic t Page -> FetchSurvey t m -> m ()
previewPage active createE =
  displayPage active Preview (renderQuestionLis (getPostAndForm <$> createE))

submitPage :: MonadWidget t m => Dynamic t Page -> Event t SurveyContent -> m ()
submitPage active surveyE = 
  displayPage active Submit (submitForm surveyE)

allPages :: MonadWidget t m => m ()
allPages = do
  rec
    pageStatus <- foldDyn ($) initialPage (leftmost [navClick, surCreate])
    let curPage = activated <$> pageStatus
    navClick <- fmap (clickActive <$>) (breadCrumb pageStatus)
    (newStatus, createE) <- homePage curPage
    let surCreate = const <$> newStatus
  submitPage curPage (getContent <$> createE)
  previewPage curPage createE

initialPage :: PageStatus
initialPage = PageStatus [Home] Home

clickActive :: Page -> PageStatus -> PageStatus
clickActive act (PageStatus ps _) = PageStatus ps act

displayPage :: MonadWidget t m => Dynamic t Page -> Page -> m a -> m a
displayPage focus current w =
  divDynClass tabClass w
  where
    getTab foc
      | foc == current = "ui active tab"
      | otherwise = "ui tab"
    tabClass = getTab <$> focus
