{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (Text)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NodeList
import GHCJS.DOM.Types hiding (Text)
import React

div_ = "div" :: Text
span_ = "span" :: Text

main = postGUIAsync $ do
  (Just doc) <- currentDocument
  (Just b) <- documentGetBody doc
  runReact $ do
    let comp = component $ do
          putStrLn "Rendering the thing"
          return $ createElement div_ props [str_ "Hello, World~"]
    hello <- createClass (comp { componentDisplayName = Just "Hello" })
    renderOn (castToElement b) $ createElement div_ props
      [ str_ "Wibble"
      , elem_ div_ props [str_ "Wobble"]
      , elem_ hello props []
      , elem_ span_ props [str_ "Span!"]
      ]
  return ()

