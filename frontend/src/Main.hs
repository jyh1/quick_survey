{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom
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

import Common
import Types

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 semanticCSS) $ divClass "ui container" $ do
  importExternalJS
  inputConfig <- loadingFile
  let qLisE = fmapMaybe jsonToQuestion inputConfig
  let parsedQs = parseSurvey <$> qLisE
  -- buildE <- getPostBuild
  serventS <- connectServant
  let qLis =  leftmost [
                  -- (parseSurvey testQuestion) <$ buildE
                  parseSurvey <$> serventS
                , parsedQs
              ]
  response <- renderQuestionLis qLis
  responseHistory <- (foldDyn (:) [] response)
  display responseHistory

importExternalJS :: MonadWidget t m => m ()
importExternalJS = liftJSM $ do
  _ <- eval jqueryJS
  _ <- eval semanticJS
  return ()

connectServant :: forall t m.MonadWidget t m => m (Event t [Question])
connectServant = do
  let getsurvey = client (Proxy :: Proxy QuestionAPI)
                          (Proxy :: Proxy m)
                          (Proxy :: Proxy ())
                          (constDyn (BaseFullUrl Http "localhost" 8081 "/"))
      sid = constDyn (QParamSome "whatever")
  pb <- getPostBuild
  surveys <- getsurvey sid pb
  return (fmapMaybe fromReqRes surveys)

fromReqRes :: ReqResult tag a -> Maybe a
fromReqRes (ResponseSuccess _ s _) = Just s
fromReqRes _ = Nothing

renderReqRes :: MonadWidget t m => ReqResult tag [Question] -> m ()
renderReqRes (ResponseSuccess _ s _) = text (Types.tshow s)
renderReqRes _ = text "error!"
  