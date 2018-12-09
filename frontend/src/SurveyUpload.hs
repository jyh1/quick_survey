{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module SurveyUpload(submitForm) where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Monoid ((<>))

import FrontendCommon
import Request


formHeader :: MonadWidget t m => m ()
formHeader = 
  elClass "h2" "ui dividing header" $ do
    elClass "i" "settings icon" blank
    divClass "content" $ do
      text "Survey Settings"
      divClass "sub header" (text "Manage your preferences")

labeledField :: MonadWidget t m => T.Text -> m (Dynamic t (Maybe T.Text))
labeledField l = divClass "field" $ do
  el "label" (text l)
  (inp, _, _) <- divClass "field" $ (surveyNameInput l)
  return inp

surveyNameForm :: MonadWidget t m => Dynamic t Bool -> m (Event t T.Text)
surveyNameForm err = divClass "ui form" $ do
  savedName <- labeledField "Survey Name"
  labeledField "Password (Optional)"
  labeledField "Admin Name"
  (e, _) <- elClass' "div" "ui orange button" (text "Upload Survey")
  let uploadEvent = tagMaybe (current savedName) (domEvent Click e)
  return uploadEvent

successMsg, errorMsg :: MonadWidget t m => Dynamic t T.Text -> m ()
successMsg name = divClass "ui success message" $ do
  divClass "header" (text "Upload Successful")
  el "p" (text "Your survey has been successfully uploaded")

errorMsg name = divClass "ui error message" $ do
  divClass "header" (text "Error")
  el "p" (text "This name has been taken, please try another.")

msgs :: MonadWidget t m => Dynamic t (Either T.Text T.Text) -> m ()
msgs name = do
  successMsg dynName
  errorMsg dynName
  where
    dynName = either (const "") id <$> name

formInformation :: MonadWidget t m => SurveyContent -> m (Dynamic t Bool, Dynamic t Bool)
formInformation survey = do
  formHeader
  clickE <- surveyNameForm undefined
  surveyName <- eventToParams clickE
  msgs surveyName
  let (_, _, postRes) = ajaxFunctions surveyName
  (succ, fail) <- postRes (constDyn (Right survey)) (() <$ clickE)
  errorStatus <- foldDyn const False (andMerge [False <$ succ, True <$ fail])
  succStatus <- foldDyn const False (orMerge [False <$ fail, True <$ succ])
  return (succStatus, errorStatus)

formWithMsgs :: MonadWidget t m => SurveyContent -> m ()
formWithMsgs survey = do
  rec
    (succStat, failStat) <- divDynClass (zipDynWith formClass succStat failStat) $ 
      formInformation survey
  return ()
  where
    boolClass name True = name
    boolClass _ False = ""
    formClass s f = "ui form " <> (boolClass "success" s) <> (boolClass "error" f)

submitForm :: MonadWidget t m => Event t SurveyContent -> m ()
submitForm survey = 
    widgetHold_ blank (formWithMsgs <$> survey)