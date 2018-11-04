{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Request where

import Servant.API
import Servant.Reflex
import Reflex.Dom
import qualified Data.Text as T
import Data.Proxy


import Types

eventToParams :: MonadWidget t m => Event t a -> m (Dynamic t (Either T.Text a))
eventToParams e = holdDyn (Left "None") (Right <$> e)

type GetSurvey t m = Event t () -> m (Event t SurveyContent, Event t T.Text)
type PostResponse t m = Dynamic t T.Text -> FieldID -> Event t ElementResponse -> m (Event t ElementResponse)

ajaxFunctions :: forall t m. MonadWidget t m => Dynamic t T.Text -> 
    (
        GetSurvey t m
      , PostResponse t m
    )
ajaxFunctions sid =
    let requestFunc = client (Proxy :: Proxy API)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BaseFullUrl Http "localhost" 8081 "/"))
        getsurvey :<|> _ :<|> postUpdate = requestFunc (Right <$> sid)

        fetchSurvey fireE = do
            res  <- getsurvey fireE
            return (fmapMaybe reqSuccess res, fmapMaybe reqFailure res)
        updateFun user field eleRes = do
            let idParam = constDyn (Right field)
            contentParam <- eventToParams eleRes
            postToServer <- postUpdate idParam (Right <$> user) contentParam (() <$ eleRes)
            return (fmapMaybe reqSuccess postToServer)
    in
        (fetchSurvey, updateFun)

