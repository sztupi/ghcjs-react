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
  componentContext,
  currentState,
  setState,
  replaceState,
  currentProps,
  setProps,
  replaceProps,
  forceUpdate,
  getDOMNode,
  isMounted,
  eventHandler,
  unsafeEventHandler,
  module React.Types,
  module React.DOM,
  module React.Props
) where
import Control.Exception
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
import GHCJS.Prim (isUndefined)
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

componentContext :: Monad m => ComponentT m (JSObject ())
componentContext = ask

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

-- | Create an event handler that can be attached to props. Callbacks marshalled via this function are managed by the component that they are created in. The callbacks will be relased in any of the following phases: componentWillUnmount, componentDidUpdate. Note: callbacks are managed in the component state using the @h$retained@ key, so be sure to not overrwrite this key on accident!
eventHandler :: FromJSRef a => (a -> ComponentT IO ()) -> ComponentT IO (JSFun (JSRef a -> IO ()))
eventHandler f = do
  ctxt <- componentContext
  inner <- liftIO $ syncCallback1 AlwaysRetain True $ \r -> do
    mx <- fromJSRef r
    case mx of
      Nothing -> throw InvalidEventException
      Just x -> runReaderT (f x) ctxt
  st <- currentState
  rs <- liftIO $ getProp ("h$retained" :: JSString) st
  liftIO $ pushArray inner rs
  return inner

unsafeEventHandler :: FromJSRef a => (a -> ComponentT IO ()) -> ComponentT IO (JSFun (JSRef a -> IO ()))
unsafeEventHandler f = do
  ctxt <- componentContext
  inner <- liftIO $ syncCallback1 NeverRetain True $ \r -> do
    mx <- fromJSRef r
    case mx of
      Nothing -> throw InvalidEventException
      Just x -> runReaderT (f x) ctxt
  return inner

component :: ComponentT IO ReactElement -> ComponentSpecification IO ps st
component f = ComponentSpecification f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

wrapCallback f = do
  cb <- syncCallback2 AlwaysRetain False $ \this x -> do
    res <- runReaderT f this
    r <- toJSRef res
    setProp ("result" :: JSString) r x
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


cleanUp f = do
  f
  s <- currentState
  liftIO $ do
    rArray <- getProp ("h$retained" :: JSString) s
    refs <- popAll rArray
    mapM_ release refs

-- createClass :: (S.MonadMask m, MonadIO m) => ComponentSpecification m ps st
                                          -- -> ReactT m (ComponentFactory m st)
createClass :: (S.MonadMask m, MonadIO m) => ComponentSpecification IO ps st -> ReactT m (ComponentFactory m' st)
createClass = fmap snd . createClass'

createClass' :: (S.MonadMask m, MonadIO m) => ComponentSpecification IO ps st
                                           -> ReactT m (ReactT m (), ComponentFactory m' st)
createClass' c = do
  o <- liftIO newObj

  f <- liftIO $ do
    (wrapped, inner) <- wrapCallback $ c ^. render
    setProp ("render" :: JSString) wrapped o
    return inner

  ifM (componentSpecificationDisplayName c) $ \n ->
    liftIO $ setProp ("displayName" :: JSString) n o

  initialStateFun <- case componentSpecificationGetInitialState c of
    -- if there's not a default state function, provide a default that
    -- provides the h$retained array
    Nothing -> do
      (wrapped, inner) <- liftIO $ wrapCallback $ liftIO $ do
        o <- newObj
        arr <- newArray
        setProp ("h$retained" :: JSString) arr o
        return o
      S.register $ liftIO $ release inner
      return wrapped
    -- otherwise, tack h$retained on to the provided state value.
    Just f -> do
      (wrapped, inner) <- liftIO $ wrapCallback $ do
        st <- f
        arr <- liftIO newArray
        liftIO $ setProp ("h$retained" :: JSString) arr st
        return st
      S.register $ liftIO $ release inner
      return wrapped

  liftIO $ setProp ("getInitialState" :: JSString) initialStateFun o

  ifM (componentSpecificationGetDefaultProps c) $ \f -> do
    (wrapped, inner) <- liftIO $ wrapCallback f
    liftIO $ setProp ("getDefaultProps" :: JSString) wrapped o
    S.register $ liftIO $ release inner
    return ()

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
      syncCallback3 AlwaysRetain False (\this x y -> runReaderT (cleanUp $ f x y) this)
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
      cb <- syncCallback1 AlwaysRetain False (runReaderT $ cleanUp f)
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

popAll :: JSArray a -> IO [JSRef a]
popAll ref = go []
  where
    go xs = do
      x <- popArray ref
      if isUndefined x
        then return xs
        else go (x : xs)

