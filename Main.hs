{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Lens hiding (createClass)
import Data.Text (Text)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NodeList
import GHCJS.DOM.Types hiding (Text)
import React
import React.Attributes
import React.DOM

main = do
  (Just doc) <- currentDocument
  (Just b) <- documentGetBody doc
  runReact $ do
    let comp = component $ do
          putStrLn "Rendering the thing"
          return $ div_ props [str_ "Hello, World~"]
    hello <- createClass (comp { componentDisplayName = Just "Hello" })
    renderOn (castToElement b) $ div_ props
      [ str_ "Wibble"
      , div_ (props & className ?~ "fancy") [str_ "Wobble"]
      , elem_ hello props []
      , span_ props [str_ "Span!"]
      , link_ (props & rel ?~ "stylesheet" & href ?~ "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css") [str_ ""]
      ]
  return ()

