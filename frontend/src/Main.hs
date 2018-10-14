{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings #-}

import Reflex.Dom
import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Marshal
import JSDOM.Types hiding (Text, Event)
import JSDOM.Generated.FileReader
import JSDOM.Types (File)
import JSDOM.EventM
import Reflex.Dom.SemanticUI
import Data.Map as Map
import Language.Javascript.JSaddle.Evaluate
import Data.Text.Encoding (encodeUtf8)

import Datatype

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 semanticCSS) $ divClass "ui container" $ do
  liftJSM (eval jqueryJS)
  liftJSM (eval semanticJS)
  header
  filesDyn <- value <$> fileInput def
  qLisE <- dataURLFileReader . fmapMaybe listToMaybe . updated $ filesDyn
  qLis <- holdDyn [] qLisE
  simpleList qLis displayQuestion

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


entries :: MonadWidget t m => Dynamic t (Map (Maybe Int) (DropdownItemConfig m))
entries = constDyn . fromList $ entry <$> (Nothing : (Just <$> [0..4]))
  where -- entry :: Maybe Int -> (Maybe Int,DropdownItemConfig m)
        entry n =
          (n, DropdownItemConfig (spell n) $
              elAttr "div" ("style" =: style) $ do
                elAttr "span" ("style" =: "font-weight: bold;") $ text $ tshow n
                elAttr "span" ("style" =: "font-style: italic;")   $ text $ spell n
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


displayOption ::(PostBuild t m, DomBuilder t m) => Dynamic t T.Text -> m ()
displayOption opt = el "label" $ do
  checkbox False def
  dynText opt
  return ()

displayQuestion :: (MonadWidget t m) => Dynamic t Question -> m ()
displayQuestion que = do
  el "br" blank
  dynText (content <$> que)
  el "br" blank
  simpleList (options <$> que) displayOption
  return ()



dataURLFileReader :: MonadWidget t m => Event t File -> m (Event t [Question])
dataURLFileReader request =
  do fileReader <- liftJSM newFileReader
     performEvent_ (fmap (\f -> readAsText fileReader (Just f) (Just "UTF8"::Maybe Text)) request)
     e <- wrapDomEvent fileReader (`on` load) $
       liftJSM (getResult fileReader >>= toJSVal >>= fromJSVal)
     
     return (fmapMaybe parseQuestion (fmapMaybe id e))