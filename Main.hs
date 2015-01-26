{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Lens hiding (createClass)
import Control.Lens.Action
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NodeList
import GHCJS.DOM.Types hiding (Text)
import Pipes.Safe
import React
import React.Props
import React.DOM
import React.Raw (debugger)

getBody :: IO (Maybe DOMElement)
getBody = currentDocument ^!? acts._Just.act documentGetBody._Just.to castToElement

printMountInfo = isMounted >>= liftIO . print

helloComponent :: ComponentSpecification IO ps st
helloComponent = component render
  & displayName ?~ "Hello"
  & willMount ?~ printMountInfo
  & didMount ?~ printMountInfo
  & willUnmount ?~ (liftIO $ putStrLn "Will unmount")
  & shouldUpdate ?~ (\_ _ -> return True)
  & didUpdate ?~ (\_ _ -> liftIO $ putStrLn "Updated!")
  where
    render = do
      ps <- currentProps
      h <- eventHandler $ \event -> do
        liftIO $ print $ mouseEventButton event
      n <- liftIO $ ps ^!? at "number" . _Just . to fromJSString
      let name = fromMaybe "no number in props!" n
      return $ div_ (props (onClick ?~ h))
        [ "Current count: ", str_ name, "!"]

defaultProps :: MonadIO m => ComponentT m (JSRef ())
defaultProps = liftIO $ do
  o <- newObj
  count <- toJSRef (0 :: Int)
  setProp ("clicks" :: JSString) count o
  return o

clickyCounterComponent :: ComponentSpecification IO () ()
clickyCounterComponent = component render
  & displayName ?~ "Clicky"
  & getDefaultProps ?~ defaultProps
  where
    render = do
      clickHandler <- unsafeEventHandler $ \_ -> do
        ps <- currentProps
        (counter :: Maybe Int) <- ps ^! readRef "clicks"
        case counter of
          Nothing -> do
            r <- liftIO $ toJSRef (0 :: Int)
            setProps (H.singleton "clicks" $ castRef r)
          Just x -> do
            r <- liftIO $ toJSRef (x + 1)
            setProps (H.singleton "clicks" $ castRef r)
      ps <- currentProps
      r <- ps ^! readRef "clicks"
      let str = str_ $ case r of
            Nothing -> "No number :("
            Just x -> pack $ show (x :: Int)
      return $ div_ noProps
        [ elem_ $ button_ (props (onClick ?~ clickHandler)) [str_ "Click me!"]
        , elem_ $ div_ noProps ["Count: ", str]
        ]

page hello ps = div_ ps
  [ elem_ $ createElement hello ps
  , elem_ $ link_ (props ((rel ?~ "stylesheet") . (href ?~ "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"))) []
  ]

main = do
  (Just e) <- getBody
  runReact $ do
    clicky <- createClass clickyCounterComponent
    let n = createElement clicky noProps
    renderOn e n
