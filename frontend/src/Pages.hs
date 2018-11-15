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
  (displayPage active Preview renderQuestionLis) createE

allPages :: MonadWidget t m => m ()
allPages = do
  rec
    pageStatus <- foldDyn ($) initialPage (leftmost [navClick, surCreate])
    let curPage = activated <$> pageStatus
    navClick <- fmap (clickActive <$>) (breadCrumb pageStatus)
    (newStatus, createE) <- homePage curPage
    let surCreate = const <$> newStatus
  displayPage curPage Submit submitForm (snd <$> createE)
  previewPage curPage createE

initialPage :: PageStatus
initialPage = PageStatus [Home] Home

clickActive :: Page -> PageStatus -> PageStatus
clickActive act (PageStatus ps _) = PageStatus ps act

displayPage :: Reflex t => Dynamic t Page -> Page -> (Dynamic t Bool -> a) -> a
displayPage focus current f =
  f ((== current) <$> focus)
