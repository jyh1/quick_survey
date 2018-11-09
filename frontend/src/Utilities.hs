{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module Utilities where

import Data.Text (Text)
import Reflex.Dom.Core
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))


toggleHide :: MonadWidget t m => Dynamic t Bool -> Text -> Dynamic t (Map.Map Text Text) -> m a -> m a
toggleHide hide tag attrs = 
    elDynAttr tag (zipDynWith (Map.insertWith (<>) "style") (hideStyle <$> hide) attrs)
        where 
            hideStyle False = ";visibility:hidden"
            hideStyle True = ""

hideDynDivClass :: MonadWidget t m => Dynamic t Bool -> Dynamic t Text -> m a -> m a
hideDynDivClass hide cls = 
    toggleHide hide "div" (("class" =:) <$> cls)