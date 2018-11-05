{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Debug (
  -- * Running JSM over WebSockets
    run
#ifndef ghcjs_HOST_OS
  , module Language.Javascript.JSaddle.WebSockets
#endif
) where

#ifndef ghcjs_HOST_OS
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
import Application
#endif

-- | Run the given 'JSM' action as the main entry point.  Either directly
--   in GHCJS or as a Warp server on the given port on GHC.
#ifdef ghcjs_HOST_OS
run :: Int -> IO () -> IO ()
run _port = id
#else
run :: Int -> JSM () -> IO ()
run port f = do
    serverApp <- mkApp ":memory:"
    app <- jsaddleWithAppOr defaultConnectionOptions (f >> syncPoint) serverApp
    runSettings (setPort port (setTimeout 3600 defaultSettings)) app
#endif