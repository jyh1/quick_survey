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

instance PersistField Question where
    toPersistValue q = PersistByteString $ toStrict (encode q)
    fromPersistValue (PersistByteString s) = 
        case eitherDecodeStrict s of
            Left s -> Left $ T.pack s
            Right q -> Right q
    fromPersistValue _ = Left "Model.hs: Error trying to deserialize a Question"


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Survey
    name T.Text
    content [Question]
    UniqueSurveyName name
    deriving Show
|]
