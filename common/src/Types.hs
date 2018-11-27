{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses     #-}


module Types where

import Data.Aeson
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Lazy (toStrict, fromStrict)
import GHC.Generics
import           Servant.API
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Arrow (left)

-- data Question = Question {
--       content :: T.Text
--     , options  :: [T.Text]
--     } deriving (Generic, Show)

-- instance ToJSON Question where
--     toEncoding = genericToEncoding defaultOptions

-- instance FromJSON Question

-- type SurveyContent = [Question]
type SurveyContent = BL.ByteString


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
             ( Get '[OctetStream] (Either T.Text SurveyContent))
        :<|> ( ReqBody '[OctetStream] SurveyContent :> Post '[JSON] SavedStatus)
        :<|> ( Capture "fieldid" FieldID :> Capture "username" T.Text :> ReqBody '[JSON] ElementResponse :> Post '[JSON] ElementResponse)
      )

-- leftPrefix :: B.ByteString
-- leftPrefix = B.pack [19, 93, 10, 27]

leftPrefixLazy :: BL.ByteString
leftPrefixLazy = BL.pack [19, 93, 10, 27]

instance MimeUnrender OctetStream (Either T.Text SurveyContent) where
    mimeUnrender _ bs = 
        case BL.stripPrefix leftPrefixLazy bs of
            Nothing -> Right (Right  bs)
            Just err -> Left <$> left show (toStrict <$> (decodeUtf8' err))

instance MimeRender OctetStream (Either T.Text SurveyContent) where
       mimeRender _ (Right bs) = bs
       mimeRender _ (Left err) = BL.append leftPrefixLazy (encodeUtf8 (fromStrict err))
   

type StaticAPI = "static" :> Raw

type API = SurveyAPI

