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
main = do
  [port] <- getArgs
  let portN = read port
  runServer portN "test.sqlite"

runServer :: Int -> T.Text -> IO ()
runServer portN sqliteFile =
  run portN =<< (simpleCors <$> mkApp sqliteFile)