{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core
import qualified Data.Map as Map


import Question
import Fileinput
import Debug (run)


import Request

main :: IO ()
main = run 3003 $ mainWidgetWithHead headElement $ divClass "ui container" $ do
  inputConfig <- loadingFile
  let qLisE = fmapMaybe jsonToQuestion inputConfig
  let parsedQs = parseSurvey <$> qLisE
  let (serventS, postRes) = ajaxFunctions (constDyn "test")
  (testSurveys, _) <- getPostBuild >>= serventS
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