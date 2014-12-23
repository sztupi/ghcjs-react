module React.Raw where
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import           GHCJS.Types
import           React.Types

foreign import javascript unsafe "React.createClass($1)"
  jsCreateClass :: JSObject (ComponentSpecification m ps st) -> IO (ComponentFactory m st)

foreign import javascript unsafe "reactWrapCallback($1)"
  reactWrapCallback :: JSFun (JSRef b -> JSRef a -> IO ()) -> IO (JSFun (IO (JSRef a)))

foreign import javascript unsafe "provideThis($1)"
  provideThis :: JSFun a -> IO (JSFun (JSRef this -> a))

foreign import javascript unsafe "provideThisArb($1)"
  provideThisArb :: JSFun (JSRef this -> JSArray a -> IO c) -> IO (JSFun (JSRef this -> JSArray a -> IO c))

foreign import javascript unsafe "provideThisArbWithResult($1)"
  provideThisArbWithResult :: JSFun (JSRef this -> JSArray a -> IO c) -> IO (JSFun (JSRef this -> JSArray a -> IO c))

foreign import javascript unsafe "React.createElement.apply(null, [$1, $2].concat($3))"
  jsCreateElement :: JSRef a -> JSRef b -> JSArray c -> ReactElement

foreign import javascript unsafe "React.render($1, $2)"
  jsRender :: ReactElement -> DOMElement -> IO Component

foreign import javascript unsafe "($1).forceUpdate()"
  jsForceUpdate :: JSRef a -> IO ()

foreign import javascript unsafe "($1).getDOMNode()"
  jsGetDOMNode :: JSRef a -> IO (JSRef DOMElement)

foreign import javascript unsafe "($1).isMounted()"
  jsIsMounted :: JSRef a -> IO JSBool

foreign import javascript unsafe "($1).setProps($2)"
  jsSetProps :: JSRef a -> JSRef b -> IO ()

foreign import javascript unsafe "($1).replaceProps($2)"
  jsReplaceProps :: JSRef a -> JSRef b -> IO ()

foreign import javascript unsafe "($1).setState($2)"
  jsSetState :: JSRef a -> JSRef b -> IO ()

foreign import javascript unsafe "($1).replaceState($2)"
  jsReplaceState :: JSRef a -> JSRef b -> IO () 

foreign import javascript unsafe "$1.pop()"
  popArray :: JSArray a -> IO (JSRef a)

foreign import javascript unsafe "console.log($1)"
  debugger :: JSRef a -> IO ()

foreign import javascript unsafe "Object.keys($1)"
  objectKeys :: JSObject a -> IO (JSArray JSString)
