{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Response(responseInfo) where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Monoid ((<>))

import FrontendCommon

responseInfo :: MonadWidget t m => Event t T.Text -> m ()
responseInfo sName = do
    widgetHold_ blank (responseInfo' <$> sName)


responseInfo' :: MonadWidget t m => T.Text -> m ()
responseInfo' sId = do
    elAttr "a" attrDic (text "Responses")
    where 
        url = getResponseURL sId
        attrDic = ("href" =: url) <> ("download" =: "response.json")