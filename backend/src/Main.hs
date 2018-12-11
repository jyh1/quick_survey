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

import System.Environment

import Application

main :: IO ()
main = runServer "test.sqlite"

runServer :: T.Text -> IO ()
runServer sqliteFile = do
  [port] <- getArgs
  let portN = read port
  run portN =<< (simpleCors <$> mkApp sqliteFile)