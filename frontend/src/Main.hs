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
  header
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

  el "p" $ text "These are examples of semantic-ui widgets."
  el "p" $ uiButton (huge $ inverted $ blue def) (text "I'm a huge, inverted, blue button!")
  divClass "example" $ do
    text "Fluid selection dropdown"
    v <- semUiDropdownWithItems "test-dropdown-1"
         [DOFSelection, DOFFluid] Nothing entries mempty
    el "br" $ blank
    display v

  divClass "example" $ do
    text "Selection dropdown"
    w <- semUiDropdownWithItems "test-dropdown-2"
         [DOFSelection] Nothing entries mempty
    el "br" $ blank
    display w


importExternalJS :: MonadWidget t m => m ()
importExternalJS = liftJSM $ do
  _ <- eval jqueryJS
  _ <- eval semanticJS
  return ()

entries :: MonadWidget t m => Dynamic t (Map.Map (Maybe Int) (DropdownItemConfig m))
entries = constDyn . Map.fromList $ entry <$> (Nothing : (Just <$> [0..4]))
  where -- entry :: Maybe Int -> (Maybe Int,DropdownItemConfig m)
        entry n =
          (n, DropdownItemConfig (spell n) $
              elAttr "div" ("style" =: style) $ blank
                -- elAttr "span" ("style" =: "font-weight: bold;") $ text $ tshow n
                -- elAttr "span" ("style" =: "font-style: italic;")   $ text $ spell n
          )
        style = "display: flex; justify-content: space-between; "

spell :: Maybe Int -> Text
spell Nothing = "Favorite number"
spell (Just n)
  | n < length spellWords = spellWords !! n
  | otherwise = "Unhandled Option"
  where spellWords = ["Zero","One","Two","Three","Four"]

linkNewTab ::DomBuilder t m => Text -> Text -> m ()
linkNewTab href s =
  elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

header :: DomBuilder t m => m ()
header = do
  el "strong" $ do
    linkNewTab "https://github.com/reflex-frp/reflex-dom" "Reflex.Dom"
    text " FileInput test page"
  el "p" $ do
    text "Select an image file."



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
  -- widgetHold (text "waiting...") (renderReqRes <$> surveys)

fromReqRes :: ReqResult tag a -> Maybe a
fromReqRes (ResponseSuccess _ s _) = Just s
fromReqRes _ = Nothing

renderReqRes :: MonadWidget t m => ReqResult tag [Question] -> m ()
renderReqRes (ResponseSuccess _ s _) = text (Types.tshow s)
renderReqRes _ = text "error!"
-- getSurvey :: MonadWidget t m => Dynamic t (QParam T.Text) -> Event t () -> m (Event t (ReqResult () [Question]))
-- getSurvey = client (Proxy :: Proxy QuestionAPI)
--                           (Proxy :: Proxy m)
--                           (Proxy :: Proxy ())
--                           (constDyn (BasePath "/"))
  