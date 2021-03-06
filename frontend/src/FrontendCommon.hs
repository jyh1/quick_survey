{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
import qualified Data.IntMap.Strict as IM
-- import Control.Lens

import Types
import Common



-- Types


data Form = 
    OptionInput {optType :: OptionType, config :: OptionConfig}
  | Title {config :: OptionConfig}
  | Plain {config :: OptionConfig}
  | PlainText {config :: OptionConfig}
  | List [Form]
  deriving (Show, Eq, Read)

data OptionType = Radiogroup | Checkboxgroup
    deriving (Show, Eq, Read)

data OptionConfig = OptionConfig
    { 
        title :: T.Text, 
        radioOpts :: [T.Text], 
        colCount :: Maybe Int,
        placeholder :: Maybe T.Text
    }
    deriving (Show, Eq, Read)

defaultConf = OptionConfig "" [] Nothing Nothing
-- type ElementWithID = WithID ElementID QuestionElement
-- type ParsedQuestion = ElementWithID
emptyForm = List []

data FormState t m = FormState {post :: PostRes t m, counter :: Int, saved :: SavedRes}

type RenderElement t m a = StateT (FormState t m) m a
type RenderForm t m = RenderElement t m (Event t SurveyUpdate)

type SavedRes = IM.IntMap ElementResponse
-- The type returned by survey search
data SurveyGeneration t m = 
    SurveySearch {getPost :: (PostRes t m), getForm :: Form, unsafeGetName :: T.Text, savedResponse :: SavedRes} 
    | SurveyCreation {getPost :: (PostRes t m), getForm :: Form, unsafeGetContent :: SurveyContent}


getName :: MonadWidget t m => SurveyGeneration t m -> Maybe T.Text
getName (SurveySearch _ _ n _) = Just n
getName _ = Nothing

getContent :: MonadWidget t m => SurveyGeneration t m -> Maybe SurveyContent
getContent (SurveyCreation _ _ n) = Just n
getContent _ = Nothing

getSaved :: MonadWidget t m => SurveyGeneration t m -> SavedRes
getSaved (SurveySearch {savedResponse = s}) = s
getSaved _ = IM.empty

getRenderForm :: MonadWidget t m => SurveyGeneration t m -> (PostRes t m, Form, SavedRes)
getRenderForm sg = (getPost sg, getForm sg, getSaved sg)
type FetchSurvey t m = Event t (SurveyGeneration t m)
-- Survey creation
-- type CreateSurvey t m = Event t (PostRes t m, (Form, SurveyContent))

-- Conversion


-- Rendering Question
type PostRes t m = FieldID -> Event t ElementResponse -> m (Event t ElementResponse)

-- Types of request functions
type GetSurvey t m = Event t () -> m (Event t SurveyContent, Event t Text)
type PostResponse t m = Dynamic t (Either Text Text) -> PostRes t m
type PostSurvey t m = Dynamic t (Either Text SurveyContent) -> Event t () -> m (Event t (), Event t ())

-- Page types
data Page = Home | Preview | Submit | Survey | Responses | Example
    deriving(Show, Eq)
data PageStatus = PageStatus {pages :: [Page], activated :: Page}

exampleName :: T.Text
exampleName = "sample"

divDynClass :: MonadWidget t m => Dynamic t Text -> m a -> m a
divDynClass = elDynClass "div" 

andMerge, orMerge :: Reflex t => [Event t Bool] -> Event t Bool
andMerge = mergeWith (&&)
orMerge = mergeWith (||)

eqDyn :: (Eq a, Reflex t) => Dynamic t a -> Dynamic t a -> Dynamic t Bool
eqDyn = zipDynWith (==)

makePageStatus :: Reflex t => Dynamic t [Page] -> Dynamic t Page -> Dynamic t PageStatus
makePageStatus = zipDynWith PageStatus

{-# INLINABLE toggleHide #-}
toggleHide :: MonadWidget t m => Dynamic t Bool -> Text -> Dynamic t (Map.Map Text Text) -> m a -> m a
toggleHide hide tag attrs = 
    elDynAttr tag (zipDynWith (Map.insertWith (<>) "style") (hideStyle <$> hide) attrs)
        where 
            hideStyle False = ";display:none"
            hideStyle True = ""


{-# INLINABLE surveyNameInput #-}
surveyNameInput :: MonadWidget t m => Text -> m (Dynamic t (Maybe Text), Event t (), TextInput t)
surveyNameInput placeholder = do
    textIn <- textInput (def & attributes .~ inputAttribute)
    return (trimInput <$> (value textIn), () <$ (_textInput_input textIn), textIn)
    where
        inputAttribute = constDyn (("spellcheck" =: "false") <> ("placeholder" =: placeholder))
        trimInput b = 
          let trimed = T.strip b in
            if T.null trimed then Nothing else Just trimed

{-# INLINABLE elClassId #-}
elClassId :: forall t m a. DomBuilder t m => Text -> Text -> Text -> m a -> m a
elClassId elementTag c eid child = 
    elAttr elementTag (("class" =: c) <> ("id" =: eid)) child

{-# INLINABLE divClassId #-}
divClassId :: forall t m a. DomBuilder t m => Text -> Text -> m a -> m a
divClassId = elClassId "div"
    