{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core hiding (Home, Submit, Reset)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Monoid ((<>))


import Question
import Fileinput
import Debug (run)
import CreateSurvey
import Tabs



main :: IO ()
main = run 3003 $ mainWidgetWithHead headElement $ divClass "ui container" $ do
  post <- getPostBuild
  rec
    tabList <- tabStatus updateE
    clickE <- breadCrumb tabList
    let updateE = leftmost [Reset [StepConfig Home Normal, StepConfig Preview Normal, StepConfig Submit Active] <$ post, 
                            Activate <$> clickE]
  createE <- createOrFetch
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


    