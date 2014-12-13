{-# LANGUAGE OverloadedStrings #-}
module React where
import Control.Monad
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

type ReactM = S.SafeT IO

runReact :: ReactM a -> IO a
runReact = S.runSafeT

ifM :: Maybe a -> (a -> IO ()) -> IO ()
ifM Nothing _ = return ()
ifM (Just x) f = f x

newtype Component = Component (JSRef Component)
newtype ComponentFactory st = ComponentFactory (JSFun (IO ReactElement))
data ComponentSpecification st = ComponentSpecification
  { componentRender         :: IO ReactElement
  , componentDisplayName    :: Maybe JSString
  }

newtype ReactElement = Element (JSObject ())

component :: IO ReactElement -> ComponentSpecification st
component f = ComponentSpecification f Nothing

foreign import javascript unsafe "React.createClass($1)"
  jsCreateClass :: JSObject (ComponentSpecification st) -> IO (ComponentFactory st)

createClass :: ComponentSpecification st
            -> ReactM (ComponentFactory st)
createClass = fmap snd . createClass'

createClass' :: ComponentSpecification st
             -> ReactM (ReactM (), ComponentFactory st)
createClass' c = do
  (f, o) <- S.liftBase $ do
    f <- syncCallback AlwaysRetain False $ componentRender c
    o <- newObj
    setProp ("render" :: JSString) f o
    ifM (componentDisplayName c) $ \n -> setProp ("displayName" :: JSString) n o
    return (f, o)
  k <- S.register $ release f
  cf <- S.liftBase $ jsCreateClass o
  return (S.release k, cf)

foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
  jsCreateElement :: JSRef a -> JSRef b -> JSArray c -> ReactElement

type ReactNode = Either Text ReactElement

type Props = HashMap Text Value

class ToReactElement e where
  createElement :: e -> Props -> [ReactNode] -> ReactElement

castChildren :: [ReactNode] -> JSArray a
castChildren = unsafePerformIO . toArray . fmap (either (castRef . toJSString) (\(Element ref) -> castRef ref))

str_ :: Text -> ReactNode
str_ = Left

elem_ :: ToReactElement e => e -> Props -> [ReactNode] -> ReactNode
elem_ e ps ns = Right $ createElement e ps ns

props :: Props
props = mempty

instance ToReactElement Text where
  createElement e ps es = jsCreateElement (toJSString e) (unsafePerformIO $ toJSRef_aeson ps) (castChildren es)

instance ToReactElement (ComponentFactory st) where
  createElement (ComponentFactory e) ps es = jsCreateElement e (unsafePerformIO $ toJSRef_aeson ps) (castChildren es)

foreign import javascript unsafe "React.render($1, $2)"
  jsRender :: ReactElement -> DOMElement -> IO Component

render :: ReactElement -> DOMElement -> ReactM Component
render e d = S.liftBase $ jsRender e d

renderOn :: DOMElement -> ReactElement -> ReactM Component
renderOn = flip render

