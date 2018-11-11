{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module FrontendCommon(
    module FrontendCommon,
    module Types,
    module Common
) where

import Data.Text (Text)
import Reflex.Dom.Core
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))

import Types
import Common



-- Types

-- The type returned by creation of survey
type FetchSurvey t m = Event t (PostRes t m, SurveyContent)
-- Rendering Question
type PostRes t m = FieldID -> Event t ElementResponse -> m (Event t ElementResponse)

-- Types of request functions
type GetSurvey t m = Event t () -> m (Event t SurveyContent, Event t Text)
type PostResponse t m = Dynamic t Text -> PostRes t m
type PostSurvey t m = Event t SurveyContent -> Event t () -> m (Event t SavedStatus)

-- Page types
data Page = Home | Preview | Submit | Survey
    deriving(Show, Eq)
data PageStatus = PageStatus {pages :: [Page], activated :: Page}


eqDyn :: (Eq a, Reflex t) => Dynamic t a -> Dynamic t a -> Dynamic t Bool
eqDyn = zipDynWith (==)

makePageStatus :: Reflex t => Dynamic t [Page] -> Dynamic t Page -> Dynamic t PageStatus
makePageStatus = zipDynWith PageStatus

toggleHide :: MonadWidget t m => Dynamic t Bool -> Text -> Dynamic t (Map.Map Text Text) -> m a -> m a
toggleHide hide tag attrs = 
    elDynAttr tag (zipDynWith (Map.insertWith (<>) "style") (hideStyle <$> hide) attrs)
        where 
            hideStyle False = ";visibility:hidden"
            hideStyle True = ""

hideDynDivClass :: MonadWidget t m => Dynamic t Bool -> Dynamic t Text -> m a -> m a
hideDynDivClass hide cls = 
    toggleHide hide "div" (("class" =:) <$> cls)