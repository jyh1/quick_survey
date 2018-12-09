{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Tabs where

import FrontendCommon(tshow)
import           Data.Monoid ((<>))
import qualified Data.Text as T

import Reflex.Dom.Core hiding (Home, Submit, Reset)

import FrontendCommon


isActive :: Bool -> T.Text
isActive True = "active item"
isActive False = "item"

breadCrumbEle :: MonadWidget t m => Dynamic t Page -> Dynamic t Page -> m (Event t Page)
breadCrumbEle activePage page = do
  (e, _) <- elDynClass' "a" activeClass (dynText (tshow <$> page))
  return (tag (current page) (domEvent Click e))
  where
    activeClass = isActive <$> (eqDyn activePage page)

breadCrumb :: MonadWidget t m => Dynamic t PageStatus -> m (Event t Page)
breadCrumb pageStat =
    divClassId "ui secondary pointing massive orange menu" "main-menu" $ do
        otherClick <- simpleList allpages (breadCrumbEle focus)
        return (switchDyn (leftmost <$> otherClick))
    where
        focus = activated <$> pageStat
        allpages = pages <$> pageStat