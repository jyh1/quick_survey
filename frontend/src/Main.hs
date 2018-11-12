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
import FrontendCommon



main :: IO ()
main = run 3003 $ mainWidgetWithHead headElement $ divClass "ui container" $ do
  post <- getPostBuild
  rec
    activePage <- foldDyn const Home clickE
    let pageStatus = makePageStatus (constDyn [Home, Preview, Submit]) activePage
    clickE <- breadCrumb pageStatus
  createE <- displayPage activePage Home createOrFetch
  (displayPage activePage Preview renderQuestionLis) createE


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


displayPage :: Reflex t => Dynamic t Page -> Page -> (Dynamic t Bool -> a) -> a
displayPage focus current f =
  f ((== current) <$> focus)
