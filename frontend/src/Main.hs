{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom

import MainWidget (bodyElement, headElement)


main :: IO ()
main = mainWidgetWithHead headElement bodyElement