module Fileinput where

import Reflex.Dom
import Data.Text (Text)
import GHCJS.Marshal
import JSDOM.Types hiding (Text, Event)
import JSDOM.Generated.FileReader
import JSDOM.Types (File)
import JSDOM.EventM
import Data.Maybe (listToMaybe)

dataURLFileReader :: MonadWidget t m => Event t File -> m (Event t Text)
dataURLFileReader request =
  do fileReader <- liftJSM newFileReader
     performEvent_ (fmap (\f -> readAsText fileReader (Just f) (Just "UTF8")) request)
     e <- wrapDomEvent fileReader (`on` load) $
       liftJSM (getResult fileReader >>= toJSVal >>= fromJSVal)
     
     return (fmapMaybe id e)

-- loading the first local file
loadingFile :: MonadWidget t m => m (Event t Text)
loadingFile = do
    filesDyn <- fileInput def
    getFileEvent filesDyn

getFileEvent :: MonadWidget t m => (FileInput d t) -> m (Event t Text)
getFileEvent fileButton = 
  dataURLFileReader . fmapMaybe listToMaybe . updated $ (value fileButton)