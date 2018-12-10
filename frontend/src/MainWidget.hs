{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module MainWidget where

import Reflex.Dom hiding (Home, Submit, Reset)
import qualified Data.Map as Map

import Pages

headElement, bodyElement :: MonadWidget t m => m ()
bodyElement = divClass "body-background" allPages


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
