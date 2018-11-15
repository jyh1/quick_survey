{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Pages where

import Reflex.Dom.Core hiding (Home, Submit, Reset)

import Question
import Fileinput
import Debug (run)
import CreateSurvey
import Tabs
import FrontendCommon

import qualified Data.Text as T
import Data.Monoid ((<>))
import Request

homePage :: MonadWidget t m => Dynamic t Page -> m (Event t PageStatus, FetchSurvey t m)
homePage active = do
  createE <- displayPage active Home createOrFetch
  let newStat = (PageStatus [Home, Preview, Submit] Preview)
  return (newStat <$ createE, createE)


previewPage :: MonadWidget t m => Dynamic t Page -> FetchSurvey t m -> m ()
previewPage active createE =
  (displayPage active Preview renderQuestionLis) createE

allPages :: MonadWidget t m => m ()
allPages = do
  rec
    pageStatus <- foldDyn ($) initialPage (leftmost [navClick, surCreate])
    let curPage = activated <$> pageStatus
    navClick <- fmap (clickActive <$>) (breadCrumb pageStatus)
    (newStatus, createE) <- homePage curPage
    let surCreate = const <$> newStatus
  displayPage curPage Submit submitForm (snd <$> createE)
  previewPage curPage createE

initialPage :: PageStatus
initialPage = PageStatus [Home] Home

clickActive :: Page -> PageStatus -> PageStatus
clickActive act (PageStatus ps _) = PageStatus ps act

displayPage :: Reflex t => Dynamic t Page -> Page -> (Dynamic t Bool -> a) -> a
displayPage focus current f =
  f ((== current) <$> focus)




formHeader :: MonadWidget t m => m ()
formHeader = 
  elClass "h2" "ui dividing header" $ do
    elClass "i" "settings icon" blank
    divClass "content" $ do
      text "Survey Settings"
      divClass "sub header" (text "Manage your preferences")

surveyNameForm :: MonadWidget t m => Dynamic t Bool -> m (Event t T.Text)
surveyNameForm err = 
  divClass "field" $ do
    el "label" (text "Survey Name")
    sName <- divClass "field" $ 
      textInput (def & attributes .~ inputAttribute)
    let savedName = trimInput <$> (value sName)
    (e, _) <- elClass' "div" "ui button" (text "Upload Survey")
    let uploadEvent = tagMaybe (current savedName) (domEvent Click e)
    return uploadEvent
  where
    inputAttribute = constDyn (("spellcheck" =: "false") <> ("placeholder" =: "Survey Name"))
    trimInput b = 
      let trimed = T.strip b in
        if T.null trimed then Nothing else Just trimed

successMsg, errorMsg :: MonadWidget t m => Dynamic t T.Text -> m ()
successMsg name = divClass "ui success message" $ do
  divClass "header" (text "Upload Successful")
  el "p" (text "Your survey has been successfully uploaded")

errorMsg name = divClass "ui error message" $ do
  divClass "header" (text "Error")
  el "p" (text "The name has been taken, please try another.")

msgs :: MonadWidget t m => Dynamic t (Either T.Text T.Text) -> m ()
msgs name = do
  successMsg dynName
  errorMsg dynName
  where
    dynName = either (const "") id <$> name

formInformation :: MonadWidget t m => SurveyContent -> m (Dynamic t Bool, Dynamic t Bool)
formInformation survey = do
  formHeader
  rec
    msgs surveyName
    clickE <- surveyNameForm undefined
    surveyName <- eventToParams clickE
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

submitForm :: MonadWidget t m => Dynamic t Bool -> Event t SurveyContent -> m ()
submitForm hide survey = 
  hideDynDivClass hide "ui tab" $ do
    widgetHold blank (formWithMsgs <$> survey)
    return ()