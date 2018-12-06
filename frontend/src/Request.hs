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


import FrontendCommon

eventToParams :: MonadWidget t m => Event t a -> m (Dynamic t (Either T.Text a))
eventToParams e = holdDyn (Left "None") (Right <$> e)


ajaxFunctions :: forall t m. MonadWidget t m => Dynamic t (Either T.Text T.Text) -> 
    (
        GetSurvey t m
      , PostResponse t m
      , PostSurvey t m
    )
ajaxFunctions sid =
    let requestFunc = client (Proxy :: Proxy SurveyAPI)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
        --   (constDyn (BaseFullUrl Http "localhost" 8081 "/server"))
          (constDyn (BasePath "/"))
        getsurvey :<|> postsurvey :<|> postUpdate = requestFunc sid

        fetchSurvey fireE = do
            res  <- getsurvey fireE
            let reqRes = fmapMaybe reqSuccess res
            return (filterRight reqRes, filterLeft reqRes)
        updateFun user field eleRes = do
            let idParam = constDyn (Right field)
            contentParam <- eventToParams eleRes
            postToServer <- postUpdate idParam user contentParam (() <$ eleRes)
            return (fmapMaybe reqSuccess postToServer)
        saveSurvey sCont fireE = do
            savedEvent <- postsurvey sCont fireE
            let rE = fmapMaybe reqSuccess savedEvent
            return (() <$ ffilter (== Success) rE, () <$ ffilter (== Failed) rE)
    in
        (fetchSurvey, updateFun, saveSurvey)


