{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans
import Control.Lens hiding (createClass)
import Data.Aeson.Lens
import Data.Maybe
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NodeList
import GHCJS.DOM.Types hiding (Text)
import React
import React.Attributes
import React.DOM

helloComponent :: ComponentSpecification st
helloComponent = component $ do
  ps <- currentProps
  nameRef <- liftIO $ getPropMaybe ("name" :: Text) ps
  let name = fromMaybe "World" $ fmap fromJSString nameRef
  return $ div_ noProps [str_ "Hello ", str_ (name :: Text), str_ "!"]

main = do
  (Just doc) <- currentDocument
  (Just b) <- documentGetBody doc
  runReact $ do
    hello <- createClass (helloComponent { componentDisplayName = Just "Hello" })
    renderOn (castToElement b) $ div_ (props id)
      [ str_ "Wibble"
      , div_ (props $ className ?~ "fancy") [str_ "Wobble"]
      , elem_ hello (props $ prop "name" ?~ "Ian") []
      , span_ noProps [str_ "Span!"]
      , link_ (props $ (rel ?~ "stylesheet") . (href ?~ "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css")) [str_ ""]
      ]
  return ()

