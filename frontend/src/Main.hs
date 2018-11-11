{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core hiding (Home, Submit)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Monoid ((<>))


import Question
import Fileinput
import Debug (run)
import CreateSurvey

import FrontendCommon(tshow)



main :: IO ()
main = run 3003 $ mainWidgetWithHead headElement $ divClass "ui container" $ do
  breadCrumb (constDyn [StepConfig Preview Normal, StepConfig Submit Active])
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

data Status = Active | Disabled | Normal
data StepConfig = 
  StepConfig {tab :: Tabs, status :: Status}

data StepChange = Reset [StepConfig] | Activate Tabs

data Tabs = Home | Preview | Submit | Survey
  deriving(Show)

statusToClass :: Status -> T.Text
statusToClass Active = "active section"
statusToClass Disabled = "disabled section"
statusToClass Normal = "section"

tabName :: Tabs -> T.Text
tabName = tshow
-- tabName Home = "Home"
-- tabName Preview = "Preview"
-- tabName Submit = "Submit"
-- tabName Survey = "Survey"

breadCrumbEle :: MonadWidget t m => Dynamic t StepConfig -> m (Event t Tabs)
breadCrumbEle config = do
  (e, _) <- elDynClass' "a" (statusToClass <$> stat) (dynText (tabName <$> t))
  return (tag (current t) (domEvent Click e))
  where
    t = tab <$> config
    stat = status <$> config

iconDivider :: MonadWidget t m => m ()
iconDivider = elClass "i" "right angle icon divider" blank

renderStep :: MonadWidget t m => Dynamic t StepConfig -> m (Event t Tabs)
renderStep config = do
  iconDivider
  breadCrumbEle config

breadCrumb :: MonadWidget t m => Dynamic t [StepConfig] -> m (Event t Tabs)
breadCrumb allTab = do
  divClass "ui breadcrumb" $ do
    homeClick <- breadCrumbEle (constDyn (StepConfig Home Normal))
    otherClick <- simpleList allTab renderStep
    return (leftmost [homeClick, switchDyn (leftmost <$> otherClick)])
    

