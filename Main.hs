{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Lens hiding (createClass)
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NodeList
import GHCJS.DOM.Types hiding (Text)
import Pipes.Safe
import React
import React.Attributes
import React.DOM

getBody :: IO (Maybe DOMElement)
getBody = currentDocument ^!? acts._Just.act documentGetBody._Just.to castToElement

printMountInfo = isMounted >>= liftIO . print

helloComponent :: ComponentSpecification st
helloComponent = component render
  & displayName ?~ "Hello"
  & willMount ?~ printMountInfo
  & didMount ?~ printMountInfo
  & willUnmount ?~ (liftIO $ putStrLn "Will unmount")
  & shouldUpdate ?~ (\_ _ -> return False)
  & didUpdate ?~ (\_ _ -> liftIO $ putStrLn "Updated!")
  where
    render = do
      ps <- currentProps
      nameRef <- liftIO $ getPropMaybe ("number" :: Text) ps
      let name = fromMaybe "no number in props!" $ fmap fromJSString nameRef
      return $ div_ noProps $ map str_ ["Current count: ", name, "!"]

page hello ps = div_ ps
  [ elem_ $ createElement hello ps
  , elem_ $ link_ (props ((rel ?~ "stylesheet") . (href ?~ "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"))) []
  ]

main = do
  (Just e) <- getBody
  (flip runStateT) (0 :: Int) $ runReact $ do
    hello <- createClass helloComponent
    forever $ do
      counter <- lift get
      renderOn e $ page hello (props (prop "number" ?~ (toJSString $ show counter)))
      lift $ put (counter + 1)
      liftIO $ threadDelay 1000000
