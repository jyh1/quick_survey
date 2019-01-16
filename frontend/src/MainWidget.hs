{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables, CPP #-}

module MainWidget where

import Reflex.Dom hiding (Home, Submit, Reset)
import qualified Data.Map as Map
import qualified Data.Text as T

import Pages

headElement, bodyElement :: MonadWidget t m => m ()
bodyElement = divClass "body-background" allPages

customizeCSS :: T.Text
-- #ifdef ghcjs_HOST_OS
customizeCSS = "customize.css"
-- #else
-- customizeCSS = "static/customize.css"
-- #endif

headElement = do
  el "title" $ text "Quick Survey"
  styleSheet "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
  styleSheet customizeCSS
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
        -- , ("id", "bootstrap-css")
      ]) $ return ()
