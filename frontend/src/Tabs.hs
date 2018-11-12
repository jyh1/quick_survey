{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Tabs where

import FrontendCommon(tshow)
import           Data.Monoid ((<>))
import qualified Data.Text as T

import Reflex.Dom.Core hiding (Home, Submit, Reset)

import FrontendCommon


isActive :: Bool -> T.Text
isActive True = "active section"
isActive False = "section"

breadCrumbEle :: MonadWidget t m => Dynamic t Page -> Dynamic t Page -> m (Event t Page)
breadCrumbEle activePage page = do
  (e, _) <- elDynClass' "a" activeClass (dynText (tshow <$> page))
  return (tag (current page) (domEvent Click e))
  where
    activeClass = isActive <$> (eqDyn activePage page)

iconDivider :: MonadWidget t m => m ()
iconDivider = elClass "i" "right angle icon divider" blank

renderStep :: MonadWidget t m => Dynamic t Page -> Dynamic t Page -> m (Event t Page)
renderStep activePage page = do
  iconDivider
  breadCrumbEle activePage page

breadCrumb :: MonadWidget t m => Dynamic t PageStatus -> m (Event t Page)
breadCrumb pageStat =
    divClass "ui big breadcrumb" $ do
        homeClick <- breadCrumbEle focus (head <$> allpages)
        otherClick <- simpleList (tail <$> allpages) (renderStep focus)
        return (leftmost [homeClick, switchDyn (leftmost <$> otherClick)])
    where
        focus = activated <$> pageStat
        allpages = pages <$> pageStat