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

import Common
import Types

main :: IO ()
main = run 8081 (simpleCors app)



server :: Server QuestionAPI
server _ = return sampleSurvey

userAPI :: Proxy QuestionAPI
userAPI = Proxy

app :: Application
app = serve userAPI server1


