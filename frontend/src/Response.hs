{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Response(responseInfo) where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Monoid ((<>))

import FrontendCommon

responseInfo :: MonadWidget t m => Event t T.Text -> m ()
responseInfo sName = do
    widgetHold_ blank (responseInfo' <$> sName)

formHeader :: MonadWidget t m => m ()
formHeader = 
    elClass "h2" "ui dividing header" $ do
    elClass "i" "signal icon" blank
    divClass "content" $ do
        text "View Responses"

responseInfo' :: MonadWidget t m => T.Text -> m ()
responseInfo' sId = do
    formHeader
    elAttr "a" attrDic (text "Download responses")
    where 
        url = getResponseURL sId
        attrDic = 
            ("href" =: url) <> ("download" =: "response.json") <> ("class" =: "ui orange button")