{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module FrontendCommon(
    module FrontendCommon,
    module Types,
    module Common
) where

import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import qualified Data.Map.Strict as Map
import Control.Monad.State
import           Data.Monoid ((<>))
-- import Control.Lens

import Types
import Common



-- Types


data Form = 
    RadioGroup {radioTitle :: T.Text, radioOpts :: [T.Text]}
  | Title {titleTitle :: T.Text}
  | Plain T.Text
  | List [Form]
  deriving (Show, Eq, Read)

-- type ElementWithID = WithID ElementID QuestionElement
-- type ParsedQuestion = ElementWithID
type Survey = Form


data FormState t m = FormState {post :: PostRes t m, counter :: Int}

type RenderElement t m a = StateT (FormState t m) m a
type RenderForm t m = RenderElement t m (Event t SurveyUpdate)

-- The type returned by survey search
type FetchSurvey t m = Event t (PostRes t m, SurveyContent)
-- Survey creation
type CreateSurvey t m = (FetchSurvey t m, PostSurvey t m)
-- Rendering Question
type PostRes t m = FieldID -> Event t ElementResponse -> m (Event t ElementResponse)

-- Types of request functions
type GetSurvey t m = Event t () -> m (Event t SurveyContent, Event t Text)
type PostResponse t m = Dynamic t (Either Text Text) -> PostRes t m
type PostSurvey t m = Dynamic t (Either Text SurveyContent) -> Event t () -> m (Event t (), Event t ())

-- Page types
data Page = Home | Preview | Submit | Survey
    deriving(Show, Eq)
data PageStatus = PageStatus {pages :: [Page], activated :: Page}

divDynClass :: MonadWidget t m => Dynamic t Text -> m a -> m a
divDynClass = elDynClass "div" 

andMerge, orMerge :: Reflex t => [Event t Bool] -> Event t Bool
andMerge = mergeWith (&&)
orMerge = mergeWith (||)

eqDyn :: (Eq a, Reflex t) => Dynamic t a -> Dynamic t a -> Dynamic t Bool
eqDyn = zipDynWith (==)

makePageStatus :: Reflex t => Dynamic t [Page] -> Dynamic t Page -> Dynamic t PageStatus
makePageStatus = zipDynWith PageStatus

toggleHide :: MonadWidget t m => Dynamic t Bool -> Text -> Dynamic t (Map.Map Text Text) -> m a -> m a
toggleHide hide tag attrs = 
    elDynAttr tag (zipDynWith (Map.insertWith (<>) "style") (hideStyle <$> hide) attrs)
        where 
            hideStyle False = ";display:none"
            hideStyle True = ""

surveyNameInput :: MonadWidget t m => Text -> m (Dynamic t (Maybe Text), Event t (), TextInput t)
surveyNameInput placeholder = do
    textIn <- textInput (def & attributes .~ inputAttribute)
    return (trimInput <$> (value textIn), () <$ (_textInput_input textIn), textIn)
    where
        inputAttribute = constDyn (("spellcheck" =: "false") <> ("placeholder" =: placeholder))
        trimInput b = 
          let trimed = T.strip b in
            if T.null trimed then Nothing else Just trimed

-- hideDynDivClass :: MonadWidget t m => Dynamic t Bool -> Dynamic t Text -> m a -> m a
-- hideDynDivClass hide cls = 
--     toggleHide hide "div" (("class" =:) <$> cls)