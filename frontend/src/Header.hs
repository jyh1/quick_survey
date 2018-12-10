{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Header (headerElement) where

import Reflex.Dom.Core hiding (Home, Submit, Reset)
import qualified Data.Text as T
import Data.Map as M

import FrontendCommon

headerElement, titleElement :: MonadWidget t m => m ()
headerElement = do
    titleElement

titleElement = divClass "ui sticky fixed top header-container" $ do
    divClass "header-text" (text "Quick Survey")
    githubIcon
    divClass "sub-header-text" (text "Build online surveys from config files")

githubIcon :: MonadWidget t m => m ()
githubIcon = elAttr "a" labledAttr $ do
    elClassId "i" "github icon" "github-icon" blank
    where
        labledAttr = M.fromList [("href", "https://github.com/jyh1/survey")]