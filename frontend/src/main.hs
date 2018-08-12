{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Frontend

main :: IO ()
main = mainWidgetWithHead head body
    where 
  css = $(embedFile "css/bootstrap-grid.css")
  (head, body) = frontend
