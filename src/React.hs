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
  module React.Props
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
import React.Props
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

ifM :: Monad m => Maybe a -> (a -> m ()) -> m ()
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

component :: ComponentT IO ReactElement -> ComponentSpecification IO ps st
component f = ComponentSpecification f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

wrapCallback c = do
  cb <- syncCallback2 AlwaysRetain False $ \this x -> do
    (Element res) <- runReaderT (c ^. render) this
    setProp ("result" :: JSString) res x
  w <- reactWrapCallback cb 
  return (w, cb)

syncCallback3 retainStrat runAsync f = do
  inner <- syncCallback2 retainStrat runAsync $ \this rest -> do
    x <- indexArray 0 rest
    y <- indexArray 1 rest
    f this (castRef x) (castRef y)
  wrapped <- provideThisArb inner
  return (wrapped, inner)

wrapShouldUpdate retainStrat runAsync f = do
  inner <- syncCallback2 retainStrat runAsync $ \this rest -> do
    result <- indexArray 0 rest
    x <- indexArray 1 rest
    y <- indexArray 2 rest
    f this result (castRef x) (castRef y)
  wrapped <- provideThisArbWithResult inner
  return (wrapped, inner)

-- createClass :: (S.MonadMask m, MonadIO m) => ComponentSpecification m ps st
                                          -- -> ReactT m (ComponentFactory m st)
createClass :: (S.MonadMask m, MonadIO m) => ComponentSpecification IO ps st -> ReactT m (ComponentFactory m' st)
createClass = fmap snd . createClass'

createClass' :: (S.MonadMask m, MonadIO m) => ComponentSpecification IO ps st
                                           -> ReactT m (ReactT m (), ComponentFactory m' st)
createClass' c = do
  o <- liftIO newObj

  f <- liftIO $ do
    (wrapped, inner) <- wrapCallback c
    setProp ("render" :: JSString) wrapped o
    return inner

  ifM (componentSpecificationDisplayName c) $ \n ->
    liftIO $ setProp ("displayName" :: JSString) n o

  ifM (componentSpecificationWillMount c) $ \f -> do
    (wrapped, inner) <- liftIO $ do
      cb <- syncCallback1 AlwaysRetain False (runReaderT f)
      w <- provideThis cb
      return (w, cb)
    liftIO $ setProp ("componentWillMount" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

  ifM (componentSpecificationDidMount c) $ \f -> do
    (wrapped, inner) <- liftIO $ do
      cb <- syncCallback1 AlwaysRetain False (runReaderT f)
      w <- provideThis cb
      return (w, cb)
    liftIO $ setProp ("componentDidMount" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

  ifM (componentSpecificationWillReceiveProps c) $ \f -> do
    (wrapped, inner) <- liftIO $ do
      cb <- syncCallback2 AlwaysRetain False (\this x -> runReaderT (f x) this)
      w <- provideThis cb
      return (w, cb)
    liftIO $ setProp ("componentWillReceiveProps" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

  ifM (componentSpecificationShouldUpdate c) $ \f -> do
    (wrapped, inner) <- liftIO $ do
      wrapShouldUpdate AlwaysRetain False $ \this result x y -> do
        res <- runReaderT (f x y) this
        resRef <- toJSRef res
        setProp ("result" :: JSString) resRef result
    liftIO $ setProp ("shouldComponentUpdate" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

  ifM (componentSpecificationWillUpdate c) $ \f -> do
    (wrapped, inner) <- liftIO $ do
      syncCallback3 AlwaysRetain False (\this x y -> runReaderT (f x y) this)
    liftIO $ setProp ("componentWillUpdate" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

  ifM (componentSpecificationDidUpdate c) $ \f -> do
    (wrapped, inner) <- liftIO $ do
      syncCallback3 AlwaysRetain False (\this x y -> runReaderT (f x y) this)
    liftIO $ setProp ("componentDidUpdate" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

  ifM (componentSpecificationWillUnmount c) $ \f -> do
    (wrapped, inner) <- liftIO $ do
      cb <- syncCallback1 AlwaysRetain False (runReaderT f)
      w <- provideThis cb
      return (w, cb)
    liftIO $ setProp ("componentWillUnmount" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

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

eventList :: [JSString]
eventList =
  [ "onCopy"
  , "onCut"
  , "onPaste"
  , "onKeyDown"
  , "onKeyPress"
  , "onKeyUp"
  , "onFocus"
  , "onBlur"
  , "onChange"
  , "onInput"
  , "onSubmit"
  , "onClick"
  , "onDoubleClick"
  , "onDrag"
  , "onDragEnd"
  , "onDragEnter"
  , "onDragExit"
  , "onDragLeave"
  , "onDragOver"
  , "onDragStart"
  , "onDrop"
  , "onMouseDown"
  , "onMouseEnter"
  , "onMouseLeave"
  , "onMouseMove"
  , "onMouseOut"
  , "onMouseOver"
  , "onMouseUp"
  , "onTouchCancel"
  , "onTouchEnd"
  , "onTouchMove"
  , "onScroll"
  , "onWheel"
  ]

{-
retainEvents :: Props -> HashMap Text (JSFun ()) -> IO (HashMap Text (JSFun ()))
retainEvents

maybeReleaseEvents :: JSRef ps -> HashMap Text (JSFun ()) -> IO ()
maybeReleaseEvents newProps registered = forM_ eventList $ \eventName -> do
  prop <- getProp eventName newProps
  current = lookup eventName registered
  case current of
    Nothing -> 
-}
