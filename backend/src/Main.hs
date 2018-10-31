{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where


import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson.Types
import GHC.Generics
import Servant
import qualified Data.Text as T
import Network.Wai.Middleware.Cors
import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectFirst, (==.)
                                         , insert, entityVal)
import Data.Maybe(fromMaybe)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStderrLoggingT)

import Common
import Types

import Model

main :: IO ()
main = runServer ":memory:"



-- server :: Server QuestionAPI
-- server _ = return sampleSurvey

userAPI :: Proxy QuestionAPI
userAPI = Proxy

app :: ConnectionPool -> Application
app pool = serve userAPI $ server pool


mkApp :: T.Text -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool sqliteFile 5

  runSqlPool (runMigration migrateAll) pool
  flip runSqlPersistMPool pool $ 
    insert (Survey "test" sampleSurvey)
  return $ app pool

runServer :: T.Text -> IO ()
runServer sqliteFile =
  run 8081 =<< (simpleCors <$> mkApp sqliteFile)

server :: ConnectionPool -> Server QuestionAPI
server pool =
    surveyGetH
    where
        -- userAddH newUser = liftIO $ userAdd newUser
        -- userGetH name    = liftIO $ userGet name
        surveyGetH = liftIO . surveyGet

        -- userAdd :: User -> IO (Maybe (Key User))
        -- userAdd newUser = flip runSqlPersistMPool pool $ do
        -- exists <- selectFirst [UserName ==. (userName newUser)] []
        -- case exists of
        --     Nothing -> Just <$> insert newUser
        --     Just _ -> return Nothing

        surveyGet :: T.Text -> IO [Question]
        surveyGet sId = flip runSqlPersistMPool pool $ do
            mSurvey <- selectFirst [SurveyName ==. sId] []
            return $ fromMaybe [] (surveyContent <$> entityVal <$> mSurvey)