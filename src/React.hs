{-# LANGUAGE OverloadedStrings #-}
module React where
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Monoid
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Control.Lens ((^.))
import qualified Pipes.Safe as S
import System.IO.Unsafe

type DOMElement = DOM.Element
type ComponentContext = JSObject ()
type ReactM = S.SafeT IO
type ComponentT m = ReaderT ComponentContext m
type ComponentM = Reader ComponentContext

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
  { componentRender           :: ComponentT IO ReactElement
  , componentDisplayName      :: Maybe JSString
  , componentGetInitialState  :: Maybe (IO Object)
  , componentGetDefaultProps  :: Maybe (IO Object)
  -- , componentGetPropTypes 
  -- , componentMixins
  -- , componentStatics
  , componentWillMount        :: Maybe (ComponentT IO ())
  , componentDidMount         :: Maybe (ComponentT IO ())
  , componentWillReceiveProps :: Maybe (Object -> ComponentT IO ())
  , componentShouldUpdate     :: Maybe (Object -> Object -> ComponentM Bool)
  -- TODO prevent setState in this context
  , componentWillUpdate       :: Maybe (Object -> Object -> ComponentT IO ())
  , componentDidUpdate        :: Maybe (Object -> Object -> ComponentT IO ())
  , componentWillUnmount      :: Maybe (ComponentT IO ())
  }

-- TODO: should these support the optional callbacks?
setState :: Object -> ComponentT IO ()
setState = undefined

replaceState :: Object -> ComponentM ()
replaceState = undefined

forceUpdate :: ComponentM ()
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

newtype ReactElement = Element (JSObject ())

component :: ComponentT IO ReactElement -> ComponentSpecification st
component f = ComponentSpecification f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

foreign import javascript unsafe "React.createClass($1)"
  jsCreateClass :: JSObject (ComponentSpecification st) -> IO (ComponentFactory st)

foreign import javascript unsafe "reactWrapCallback($1)"
  reactWrapCallback :: JSFun (JSRef b -> JSRef a -> IO ()) -> IO (JSFun (IO (JSRef a)))

wrapCallback c = do
  cb <- syncCallback2 AlwaysRetain False $ \this x -> do
    (Element res) <- runReaderT (componentRender c) this
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
    ifM (componentDisplayName c) $ \n -> setProp ("displayName" :: JSString) n o
    return (inner, o)
  k <- S.register $ release f
  cf <- S.liftBase $ jsCreateClass o
  return (S.release k, cf)

foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
  jsCreateElement :: JSRef a -> JSRef b -> JSArray c -> ReactElement

type ReactNode = Either Text ReactElement

type Props = HashMap Text Text

class ToReactElement e where
  createElement :: e -> Maybe Props -> [ReactNode] -> ReactElement

castChildren :: [ReactNode] -> JSArray a
castChildren = unsafePerformIO . toArray . fmap (either (castRef . toJSString) (\(Element ref) -> castRef ref))

str_ :: Text -> ReactNode
str_ = Left

elem_ :: ToReactElement e => e -> Maybe Props -> [ReactNode] -> ReactNode
elem_ e ps ns = Right $ createElement e ps ns

noProps :: Maybe Props
noProps = Nothing

props :: (Props -> Props) -> Maybe Props
props f = Just $ f mempty

instance ToReactElement Text where
  createElement e ps es = jsCreateElement (toJSString e) (maybe jsNull (unsafePerformIO . toJSRef_aeson) ps) (castChildren es)

instance ToReactElement (ComponentFactory st) where
  createElement (ComponentFactory e) ps es = jsCreateElement e (maybe jsNull (unsafePerformIO . toJSRef_aeson) ps) (castChildren es)

foreign import javascript unsafe "React.render($1, $2)"
  jsRender :: ReactElement -> DOMElement -> IO Component

render :: ReactElement -> DOMElement -> ReactM Component
render e d = S.liftBase $ jsRender e d

renderOn :: DOMElement -> ReactElement -> ReactM Component
renderOn = flip render

