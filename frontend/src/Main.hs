{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core hiding (Home, Submit, Reset)
import qualified Data.Map as Map

import Debug
import Pages



main :: IO ()
main = run 3003 $ mainWidgetWithHead headElement $ divClass "body-background" $ 
  allPages


headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Survey"
  styleSheet "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
  styleSheet "static/customize.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
        , ("id", "bootstrap-css")
      ]) $ return ()
