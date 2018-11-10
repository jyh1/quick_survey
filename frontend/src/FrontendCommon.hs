{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module FrontendCommon(
    module FrontendCommon,
    module Types
) where

import Data.Text (Text)
import Reflex.Dom.Core
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))

import Types



-- Types

-- The type returned by creation of survey
type FetchSurvey t m = Event t (PostRes t m, SurveyContent)
-- Rendering Question
type PostRes t m = FieldID -> Event t ElementResponse -> m (Event t ElementResponse)

-- Types of request functions
type GetSurvey t m = Event t () -> m (Event t SurveyContent, Event t Text)
type PostResponse t m = Dynamic t Text -> PostRes t m
type PostSurvey t m = Event t SurveyContent -> Event t () -> m (Event t SavedStatus)

toggleHide :: MonadWidget t m => Dynamic t Bool -> Text -> Dynamic t (Map.Map Text Text) -> m a -> m a
toggleHide hide tag attrs = 
    elDynAttr tag (zipDynWith (Map.insertWith (<>) "style") (hideStyle <$> hide) attrs)
        where 
            hideStyle False = ";visibility:hidden"
            hideStyle True = ""

hideDynDivClass :: MonadWidget t m => Dynamic t Bool -> Dynamic t Text -> m a -> m a
hideDynDivClass hide cls = 
    toggleHide hide "div" (("class" =:) <$> cls)