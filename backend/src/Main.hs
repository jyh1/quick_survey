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
import Data.Aeson.Types hiding (Success)
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



-- server :: Server API
-- server _ = return sampleSurvey

userAPI :: Proxy API
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

server :: ConnectionPool -> Server API
server pool sName =
    surveyGetH :<|> surveyUploadH :<|> saveResponseH
    where
        surveyGetH = liftIO surveyGet
        surveyUploadH ns = liftIO (surveyUpload ns)
        saveResponseH f res = liftIO (saveResponse f res)

        surveyUpload :: SurveyContent -> IO SavedStatus
        surveyUpload newSurvey = flip runSqlPersistMPool pool $ do
          exists <- selectFirst [SurveyName ==. sName] []
          case exists of
              Nothing -> do
                _ <- insert (Survey sName newSurvey)
                return Success
              Just _ -> return Failed

        surveyGet :: IO [Question]
        surveyGet = flip runSqlPersistMPool pool $ do
            mSurvey <- selectFirst [SurveyName ==. sName] []
            return $ fromMaybe [] (surveyContent <$> entityVal <$> mSurvey)

        saveResponse :: FieldID -> ElementResponse -> IO ElementResponse
        saveResponse = undefined