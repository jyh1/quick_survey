{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom.Core hiding (Home, Submit, Reset)

import MainWidget (bodyElement, headElement)

import Debug


main :: IO ()
main = run 3003 $ (mainWidgetWithHead headElement bodyElement)