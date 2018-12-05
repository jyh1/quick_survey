{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Application(mkApp) where


import Network.Wai
import Servant
import qualified Data.Text as T
import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectFirst, (==.)
                                         , insert, entityVal, repsert)
import Data.Maybe(fromMaybe)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStderrLoggingT, logInfoN)
import Data.ByteString.Lazy (toStrict, fromStrict)

import Common
import Types

import Model


userAPI :: Proxy (API :<|> StaticAPI)
userAPI = Proxy

app :: ConnectionPool -> Application
app pool = serve userAPI $ (server pool :<|> serverStatic)

mkApp :: T.Text -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool sqliteFile 5

  runSqlPool (runMigration migrateAll) pool
  flip runSqlPersistMPool pool $ do
    -- insert (Survey "test" sampleSurvey)
    insert (Response 1 "test" (Clicked 2) "jyh1")
  return $ app pool



server :: ConnectionPool -> Server API
server pool sName =
    surveyGetH :<|> surveyUploadH :<|> saveResponseH
    where
        surveyGetH = liftIO surveyGet
        surveyUploadH ns = liftIO (surveyUpload ns)
        saveResponseH f u res= liftIO (saveResponse f u res)

        surveyUpload :: SurveyContent -> IO SavedStatus
        surveyUpload newSurvey = flip runSqlPersistMPool pool $ do
          exists <- selectFirst [SurveyName ==. sName] []
          case exists of
              Nothing -> do
                _ <- insert (Survey sName  newSurvey)
                return Success
              Just _ -> return Failed

        surveyGet :: IO (Either T.Text SurveyContent)
        surveyGet = flip runSqlPersistMPool pool $ do
            mSurvey <- selectFirst [SurveyName ==. sName] []
            return $ maybe (Left "Survey Not Found") Right (surveyContent <$> entityVal <$> mSurvey)

        saveResponse :: FieldID -> T.Text -> ElementResponse -> IO ElementResponse
        saveResponse field user res = 
          let newResponse = Response field sName res user in
            flip runSqlPersistMPool pool $ do
              repsert (ResponseKey field sName user) newResponse
              return res



-- staticAPI :: Proxy StaticAPI
-- staticAPI = Proxy

serverStatic :: Server StaticAPI
serverStatic = serveDirectoryWebApp "Semantic-UI-CSS"