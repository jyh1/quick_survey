{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Request where

import Servant.API
import Servant.Reflex
import Reflex.Dom
import qualified Data.Text as T
import Data.Proxy


import Types
import Question

eventToParams :: MonadWidget t m => Event t a -> m (Dynamic t (Either T.Text a))
eventToParams e = holdDyn (Left "None") (Right <$> e)

fromReqRes :: ReqResult tag a -> Maybe a
fromReqRes (ResponseSuccess _ s _) = Just s
fromReqRes _ = Nothing

ajaxFunctions :: forall t m. MonadWidget t m => m (Event t SurveyContent, Event t SurveyUpdate -> m (Event t ElementResponse))
ajaxFunctions =
    let requestFunc = client (Proxy :: Proxy API)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BaseFullUrl Http "localhost" 8081 "/"))
        sid = constDyn (Right "test")
        getsurvey :<|> _ :<|> postUpdate = requestFunc sid
    in
        do
            pb <- getPostBuild
            surveys <- getsurvey pb
            let
                updateFun postUpdate surveyRes = do
                    idParam <- eventToParams (getId <$> surveyRes)
                    contentParam <- eventToParams (getEle <$> surveyRes)    
                    postToServer <- postUpdate idParam (constDyn (Right "jyh1")) contentParam (() <$ surveyRes)
                    return (fmapMaybe fromReqRes postToServer)
            return ((fmapMaybe fromReqRes surveys), updateFun postUpdate)



