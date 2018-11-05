{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE DeriveGeneric               #-}

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
-- share [mkPersist sqlSettings] [persistLowerCase|
Survey
    name T.Text
    content [Question]
    Primary name
Response
    field FieldID
    name T.Text
    response ElementResponse
    user T.Text
    Foreign Survey fkparent name
    Primary field name user
    deriving Show
|]
