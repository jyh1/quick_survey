{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Example(exampleSurvey) where

import Reflex.Dom.Core
import qualified Data.Text as T
import           Data.Monoid ((<>))

import Question.Question
import Request
import FrontendCommon

exampleSurvey :: (MonadWidget t m) => m (FetchSurvey t m)
exampleSurvey = do
    ev <- getPostBuild
    let (fetch, postResponse, _) = ajaxFunctions (Right <$> (constDyn exampleName))
    (successRaw, fail) <- fetch ev
    let success = filterRight (parseSurvey <$> successRaw)
    return ((\x  -> SurveyCreation dummyPost x "") <$> success)
        where 
            dummyPost _ res = return res
