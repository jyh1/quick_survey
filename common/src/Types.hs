{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Types where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import           Servant.API

-- data Question = Question {
--       content :: T.Text
--     , options  :: [T.Text]
--     } deriving (Generic, Show)

-- instance ToJSON Question where
--     toEncoding = genericToEncoding defaultOptions

-- instance FromJSON Question

-- type SurveyContent = [Question]
type SurveyContent = Value

type WithID key ele = (key, ele)

getId = fst
getEle = snd


data ElementResponse = 
      Clicked ElementID
    | Clear
    deriving (Show, Eq, Generic)

instance ToJSON ElementResponse where 
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ElementResponse

type ElementID = Int

type FieldID = Int

type SurveyUpdate = WithID FieldID ElementResponse


data SavedStatus = Success | Failed
    deriving (Show, Generic, Eq)
instance ToJSON SavedStatus
instance FromJSON SavedStatus

type SurveyAPI = 
    "survey" :> Capture "surveyid" T.Text :>
      (
             ( Get '[JSON] (Either T.Text SurveyContent))
        :<|> ( ReqBody '[JSON] SurveyContent :> Post '[JSON] SavedStatus)
        :<|> ( Capture "fieldid" FieldID :> Capture "username" T.Text :> ReqBody '[JSON] ElementResponse :> Post '[JSON] ElementResponse)
      )

type StaticAPI = "static" :> Raw

type API = SurveyAPI

