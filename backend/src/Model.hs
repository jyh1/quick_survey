{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Data.Aeson
import qualified Data.Text as T

import Database.Persist.TH
import Database.Persist.Class
import Database.Persist.Types
import Data.ByteString.Lazy(toStrict)

import Types hiding (Survey)
import PersistentType



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Survey
    name T.Text
    content [Question]
    UniqueSurveyName name
    deriving Show
Response
    field FieldID
    survey T.Text
    response ElementResponse
    user T.Text
    -- UniqueResponse survey field user
|]
