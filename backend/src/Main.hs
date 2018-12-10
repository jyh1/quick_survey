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

import Network.Wai.Handler.Warp

import qualified Data.Text as T
import Network.Wai.Middleware.Cors

import Application

main :: IO ()
main = runServer "test.sqlite"

runServer :: T.Text -> IO ()
runServer sqliteFile =
  run 3003 =<< (simpleCors <$> mkApp sqliteFile)