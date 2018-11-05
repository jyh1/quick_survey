{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import JSDOM.Types (liftJSM)
import Reflex.Dom.SemanticUI
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Evaluate
import Data.Text.Encoding (encodeUtf8)
import Data.Proxy

import Servant.API

import Servant.Reflex


import Question
import Fileinput
import Application
import Debug (run)


import Common
import Types
import Request

main :: IO ()
main = run 3003 $ mainWidgetWithCss (encodeUtf8 semanticCSS) $ divClass "ui container" $ do
  importExternalJS
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
  -- echoBack <- postUpdate response
  responseHistory <- (foldDyn (:) [] response)
  display responseHistory

importExternalJS :: MonadWidget t m => m ()
importExternalJS = liftJSM $ do
  _ <- eval jqueryJS
  _ <- eval semanticJS
  return ()


renderReqRes :: MonadWidget t m => ReqResult tag SurveyContent -> m ()
renderReqRes (ResponseSuccess _ s _) = text (Common.tshow s)
renderReqRes _ = text "error!"