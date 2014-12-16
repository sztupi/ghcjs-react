{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module React (
  runReact,
  renderElement,
  renderOn,
  component,
  createClass,
  createClass',
  ToReactElement(..),
  elem_,
  str_,
  props,
  noProps,
  currentState,
  setState,
  replaceState,
  currentProps,
  setProps,
  replaceProps,
  forceUpdate,
  getDOMNode,
  isMounted,
  module React.Types,
  module React.DOM,
  module React.Attributes
) where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Control.Lens (Lens', (^.), (^?))
import qualified Pipes.Safe as S
import React.Attributes
import React.DOM
import React.Internal
import React.Raw
import React.Types
import System.IO.Unsafe

{- $primitives
-}

runReact :: (S.MonadMask m, MonadIO m) => ReactT m a -> m a
runReact = S.runSafeT

unsafeReadObject :: JSRef Value -> IO Object
unsafeReadObject r = do
  v <- fromJSRef r
  print v
  let (Just (Object o)) = v
  return o

ifM :: Maybe a -> (a -> IO ()) -> IO ()
ifM Nothing _ = return ()
ifM (Just x) f = f x

-- TODO: should these support the optional callbacks?
setState :: MonadIO m => State -> ComponentT m ()
setState st = do
  ctxt <- ask
  liftIO $ jsSetState ctxt (makeProps st)

replaceState :: MonadIO m => State -> ComponentT m ()
replaceState st = do
  ctxt <- ask
  liftIO $ jsReplaceState ctxt (makeProps st)

forceUpdate :: MonadIO m => ComponentT m ()
forceUpdate = ask >>= liftIO . jsForceUpdate

getDOMNode :: MonadIO m => ComponentT m (Maybe DOMElement)
getDOMNode = do
  ctxt <- ask
  me <- liftIO $ jsGetDOMNode ctxt
  return $ if isNull me
    then Nothing
    else Just $ DOM.Element $ castRef me

isMounted :: MonadIO m => ComponentT m Bool
isMounted = do
  ctxt <- ask
  liftIO $ fmap fromJSBool $ jsIsMounted ctxt

setProps :: MonadIO m => Props -> ComponentT m ()
setProps ps = do
  ctxt <- ask
  liftIO $ jsSetProps ctxt (makeProps ps)

replaceProps :: MonadIO m => Props -> ComponentT m ()
replaceProps ps = do
  ctxt <- ask
  liftIO $ jsReplaceProps ctxt (makeProps ps)

currentProps :: Monad m => ComponentT m (JSRef a)
currentProps = do
  ctxt <- ask
  return $ unsafePerformIO $ getProp ("props" :: JSString) ctxt

currentState :: Monad m => ComponentT m (JSRef a)
currentState = do
  ctxt <- ask
  return $ unsafePerformIO $ getProp ("state" :: JSString) ctxt

component :: ComponentT IO ReactElement -> ComponentSpecification st
component f = ComponentSpecification f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

wrapCallback c = do
  cb <- syncCallback2 AlwaysRetain False $ \this x -> do
    (Element res) <- runReaderT (c ^. render) this
    setProp ("result" :: JSString) res x
  w <- reactWrapCallback cb 
  return (w, cb)

createClass :: (S.MonadMask m, MonadIO m) => ComponentSpecification st
                                          -> ReactT m (ComponentFactory st)
createClass = fmap snd . createClass'

createClass' :: (S.MonadMask m, MonadIO m) => ComponentSpecification st
                                           -> ReactT m (ReactT m (), ComponentFactory st)
createClass' c = do
  (f, o) <- liftIO $ do
    o <- newObj
    (wrapped, inner) <- wrapCallback c
    setProp ("render" :: JSString) wrapped o
    ifM (componentSpecificationDisplayName c) $ \n -> setProp ("displayName" :: JSString) n o
    return (inner, o)
  k <- S.register $ liftIO $ release f
  cf <- liftIO $ jsCreateClass o
  return (S.release k, cf)

elem_ :: ReactElement -> ReactNode
elem_ = ElemNode

str_ :: Text -> ReactNode
str_ = TextNode

noProps :: Maybe Props
noProps = Nothing

props :: (Props -> Props) -> Maybe Props
props f = Just $ f mempty

renderElement :: MonadIO m => ReactElement -> DOMElement -> ReactT m Component
renderElement e d = liftIO $ jsRender e d

renderOn :: MonadIO m => DOMElement -> ReactElement -> ReactT m Component
renderOn = flip renderElement

