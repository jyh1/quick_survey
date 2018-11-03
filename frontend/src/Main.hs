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
import Request

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 semanticCSS) $ divClass "ui container" $ do
  importExternalJS
  inputConfig <- loadingFile
  let qLisE = fmapMaybe jsonToQuestion inputConfig
  let parsedQs = parseSurvey <$> qLisE
  -- buildE <- getPostBuild
  (serventS, postUpdate) <- ajaxFunctions
  let qLis =  leftmost [
                  -- (parseSurvey testQuestion) <$ buildE
                  parseSurvey <$> serventS
                , parsedQs
              ]
  response <- renderQuestionLis qLis
  echoBack <- postUpdate response
  responseHistory <- (foldDyn (:) [] echoBack)
  display responseHistory

importExternalJS :: MonadWidget t m => m ()
importExternalJS = liftJSM $ do
  _ <- eval jqueryJS
  _ <- eval semanticJS
  return ()

-- type PostSurvey t m = Dynamic t (Either Text [Question]) -> Event t () -> m (Event t (ReqResult () SavedStatus))
-- type PostResponse t m = Dynamic t (Either Text Int) -> Dynamic t (Either Text Text) -> Dynamic t (Either Text ElementResponse) -> Event t () -> m (Event t (ReqResult () ElementResponse))
-- type TestAPI = 
-- connectServant :: forall t m.MonadWidget t m => m (Event t SurveyContent, Event t SurveyUpdate -> Event t ElementResponse)
-- connectServant = do
--   let requestFunc = client (Proxy :: Proxy API)
--         (Proxy :: Proxy m)
--         (Proxy :: Proxy ())
--         (constDyn (BaseFullUrl Http "localhost" 8081 "/"))
--       sid = constDyn (Right ("test":: Text))
--       constructParams k = constDyn (Right k)
--       getsurvey :<|> _ :<|> postUpdate = requestFunc sid
--       updateFun surveyRes = postUpdate ( (getId <$> surveyRes))
--   pb <- getPostBuild
--   surveys <- getsurvey pb
--   return (fmapMaybe fromReqRes surveys)

-- getSurvey :: MonadWidget t m => m (Event t SurveyContent)
-- getSurvey = do
--   pb <- getPostBuild
--   getsurvey pb
--   return (fmapMaybe fromReqRes surveys)
--     where
--       sid = constDyn (Right ("test":: Text))
--       getsurvey :<|> _ :<|> _ = ajaxFunctions sid


renderReqRes :: MonadWidget t m => ReqResult tag SurveyContent -> m ()
renderReqRes (ResponseSuccess _ s _) = text (Common.tshow s)
renderReqRes _ = text "error!"