{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings #-}

import Reflex.Dom
import Reflex.Host.Class
import Data.Dependent.Sum
import Data.List
import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Identity
import Control.Monad.Trans
import GHCJS.Marshal
import JSDOM.Types hiding (Text, Event)
import JSDOM.Generated.FileReader
import JSDOM.Types (File, UIEvent)
import JSDOM.EventM
import Control.Monad(mapM)
import Data.Aeson

import Datatype

main :: IO ()
main = mainWidget $ do
  header
  filesDyn <- value <$> fileInput def
  qLisE <- dataURLFileReader . fmapMaybe listToMaybe . updated $ filesDyn
  qLis <- holdDyn [] qLisE
  -- el "div" . widgetHold blank . ffor urlE $ \url ->
  --   displayQuestion url
  simpleList qLis displayQuestion
  footer

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

footer :: DomBuilder t m => m ()
footer = do
  el "hr" $ return ()
  el "p" $ do
    text "The code for this example can be found in the "
    linkNewTab "https://github.com/reflex-frp/reflex-examples" "Reflex Examples"
    text " repo."

displayOption ::(PostBuild t m, DomBuilder t m) => Dynamic t T.Text -> m ()
displayOption opt = el "label" $ do
  checkbox False def
  dynText opt
  return ()

-- displayQuestion :: (PostBuild t m, DomBuilder t m) => Dynamic t Question -> m ()
displayQuestion que = do
  el "br" blank
  dynText (content <$> que)
  el "br" blank
  simpleList (options <$> que) displayOption
  return ()

-- displayRes :: (PostBuild t m, DomBuilder t m) => Result Question -> m ()
-- displayRes (Data.Aeson.Error e) = text (T.pack e)
-- displayRes (Success a) = displayQuestion a


dataURLFileReader :: MonadWidget t m => Event t File -> m (Event t [Question])
dataURLFileReader request =
  do fileReader <- liftJSM newFileReader
     performEvent_ (fmap (\f -> readAsText fileReader (Just f) (Just "UTF8"::Maybe Text)) request)
     e <- wrapDomEvent fileReader (`on` load) $
       liftJSM (getResult fileReader >>= toJSVal >>= fromJSVal)
     
     return (fmapMaybe parseQuestion (fmapMaybe id e))