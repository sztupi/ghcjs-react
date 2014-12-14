{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module React where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict
import Data.Aeson
import Data.Monoid
import Data.HashMap.Strict (HashMap, toList)
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Control.Lens (Lens', (^.), (^?))
import Control.Lens.TH
import qualified Pipes.Safe as S
import System.IO.Unsafe

newtype ReactElement = Element (JSObject ())

data ReactNode = TextNode !Text
               | ElemNode !ReactElement

type DOMElement = DOM.Element
type ComponentContext = JSObject ()
type ReactM = S.SafeT IO
type ComponentT m = ReaderT ComponentContext m
type ComponentM = Reader ComponentContext
type Prop' a = Lens' (HashMap Text JSString) a
type Prop a = Prop' (Maybe a)
type Props = HashMap Text JSString
type Element = Maybe Props -> [ReactNode] -> ReactElement

runReact :: ReactM a -> IO a
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

newtype Component = Component (JSRef Component)
newtype ComponentFactory st = ComponentFactory (JSFun (IO ReactElement))
data ComponentSpecification st = ComponentSpecification
  { componentSpecificationRender           :: ComponentT IO ReactElement
  , componentSpecificationDisplayName      :: Maybe JSString
  , componentSpecificationGetInitialState  :: Maybe (IO (JSRef st))
  , componentSpecificationGetDefaultProps  :: Maybe (IO (JSRef st))
  -- , componentGetPropTypes 
  -- , componentMixins
  -- , componentStatics
  , componentSpecificationWillMount        :: Maybe (ComponentT IO ())
  , componentSpecificationDidMount         :: Maybe (ComponentT IO ())
  , componentSpecificationWillReceiveProps :: Maybe (JSRef st -> ComponentT IO ())
  , componentSpecificationShouldUpdate     :: Maybe (JSRef st -> JSRef st -> ComponentM Bool)
  -- TODO prevent setState in this context
  , componentSpecificationWillUpdate       :: Maybe (JSRef st -> JSRef st -> ComponentT IO ())
  , componentSpecificationDidUpdate        :: Maybe (JSRef st -> JSRef st -> ComponentT IO ())
  , componentSpecificationWillUnmount      :: Maybe (ComponentT IO ())
  }

makeFields ''ComponentSpecification

-- TODO: should these support the optional callbacks?
setState :: Object -> ComponentT IO ()
setState = undefined

replaceState :: Object -> ComponentM ()
replaceState = undefined

forceUpdate :: ComponentT m ()
forceUpdate = undefined

getDOMNode :: ComponentM DOMElement
getDOMNode = undefined

isMounted :: ComponentM Bool
isMounted = undefined

setProps :: Object -> ComponentM ()
setProps = undefined

replaceProps :: Object -> ComponentM ()
replaceProps = undefined

currentProps :: Monad m => ComponentT m (JSRef a)
currentProps = do
  ctxt <- ask
  return $ unsafePerformIO $ getProp ("props" :: JSString) ctxt

currentState :: ComponentM Object
currentState = undefined


component :: ComponentT IO ReactElement -> ComponentSpecification st
component f = ComponentSpecification f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

foreign import javascript unsafe "React.createClass($1)"
  jsCreateClass :: JSObject (ComponentSpecification st) -> IO (ComponentFactory st)

foreign import javascript unsafe "reactWrapCallback($1)"
  reactWrapCallback :: JSFun (JSRef b -> JSRef a -> IO ()) -> IO (JSFun (IO (JSRef a)))

wrapCallback c = do
  cb <- syncCallback2 AlwaysRetain False $ \this x -> do
    (Element res) <- runReaderT (c ^. render) this
    setProp ("result" :: JSString) res x
  w <- reactWrapCallback cb 
  return (w, cb)

createClass :: ComponentSpecification st
            -> ReactM (ComponentFactory st)
createClass = fmap snd . createClass'

createClass' :: ComponentSpecification st
             -> ReactM (ReactM (), ComponentFactory st)
createClass' c = do
  (f, o) <- S.liftBase $ do
    o <- newObj
    (wrapped, inner) <- wrapCallback c
    setProp ("render" :: JSString) wrapped o
    ifM (componentSpecificationDisplayName c) $ \n -> setProp ("displayName" :: JSString) n o
    return (inner, o)
  k <- S.register $ release f
  cf <- S.liftBase $ jsCreateClass o
  return (S.release k, cf)

foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
  jsCreateElement :: JSRef a -> JSRef b -> JSArray c -> ReactElement


class ToReactElement e f | e -> f where
  createElement :: e -> f -- Maybe Props -> [ReactNode] -> ReactElement

instance ToReactElement ReactElement ReactElement where
  createElement = id

makeProps :: Props -> JSRef a
makeProps ps = unsafePerformIO $ do
  o <- newObj
  mapM_ (\(k, v) -> setProp k v o) (toList ps)
  return o

instance ToReactElement Text (Maybe Props -> [ReactNode] -> ReactElement) where
  createElement e ps es = jsCreateElement (toJSString e) (maybe jsNull makeProps ps) (castChildren es)

instance ToReactElement (ComponentFactory st) (Maybe Props -> ReactElement) where
  createElement (ComponentFactory e) ps = jsCreateElement e (maybe jsNull makeProps ps) (castChildren [])

castChildren :: [ReactNode] -> JSArray a
castChildren = unsafePerformIO . toArray . fmap mkNodeRef

elem_ :: ReactElement -> ReactNode
elem_ = ElemNode

str_ :: Text -> ReactNode
str_ = TextNode

noProps :: Maybe Props
noProps = Nothing

props :: (Props -> Props) -> Maybe Props
props f = Just $ f mempty

foreign import javascript unsafe "React.render($1, $2)"
  jsRender :: ReactElement -> DOMElement -> IO Component

renderElement :: ReactElement -> DOMElement -> ReactM Component
renderElement e d = S.liftBase $ jsRender e d

renderOn :: DOMElement -> ReactElement -> ReactM Component
renderOn = flip renderElement

mkNodeRef :: ReactNode -> JSRef a
mkNodeRef (ElemNode (Element ref)) = castRef ref
mkNodeRef (TextNode t) = castRef $ toJSString t

