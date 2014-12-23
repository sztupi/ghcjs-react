{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
module React.Internal where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict (HashMap, toList, fromList)
import Data.Text (Text)
import GHCJS.DOM.Types (Event(..), EventTarget(..))
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import React.Raw
import React.Types
import System.IO.Unsafe

mkNodeRef :: ReactNode -> JSRef a
mkNodeRef (ElemNode (Element ref)) = castRef ref
mkNodeRef (TextNode t) = castRef $ toJSString t

class ToReactElement e f | e -> f where
  createElement :: e -> f

instance ToReactElement ReactElement ReactElement where
  createElement = id

popAll :: JSArray a -> IO [JSRef a]
popAll ref = go []
  where
    go xs = do
      x <- popArray ref
      if isUndefined x
        then return xs
        else go (x : xs)

makeProps :: Props -> JSRef a
makeProps ps = unsafePerformIO $ do
  o <- newObj
  mapM_ (\(k, v) -> setProp k v o) (toList ps)
  return o

readProps :: JSObject a -> IO Props
readProps o = do
  karray <- objectKeys o
  ks <- popAll karray
  kvs <- forM (fmap castRef ks) $ \k -> do
    v <- getProp k o
    return $! (fromJSString k, v)
  return $! fromList kvs

instance ToReactElement Text (Maybe Props -> [ReactNode] -> ReactElement) where
  createElement e ps es = jsCreateElement (toJSString e) (maybe jsNull makeProps ps) (castChildren es)

instance ToReactElement (ComponentFactory m st) (Maybe Props -> ReactElement) where
  createElement (ComponentFactory e) ps = jsCreateElement e (maybe jsNull makeProps ps) (castChildren [])

castChildren :: [ReactNode] -> JSArray a
castChildren = unsafePerformIO . toArray . fmap mkNodeRef

objProp :: FromJSRef a => JSString -> JSRef o -> MaybeT IO a
objProp p o = do
  ref <- liftIO $ getProp p o
  res <- liftIO $ fromJSRef ref
  case res of
    Nothing -> do
      liftIO $ debugger p
      liftIO $ debugger ref
      MaybeT $ return Nothing
    Just v -> return v

instance FromJSRef Event where
  fromJSRef = return . Just . Event

instance FromJSRef EventTarget where
  fromJSRef = return . Just . EventTarget

instance FromJSRef a => FromJSRef (Maybe a) where
  fromJSRef r = if isNull r || isUndefined r
    then return $ Just Nothing
    else do
      res <- fromJSRef (castRef r :: JSRef a)
      case res of
        Nothing -> return Nothing
        Just r -> return $ Just r

commonEventData o e = e
  <$> objProp "bubbles" o
  <*> objProp "cancelable" o
  <*> objProp "currentTarget" o
  <*> objProp "defaultPrevented" o
  <*> objProp "eventPhase" o
  <*> objProp "isTrusted" o
  <*> objProp "nativeEvent" o
  -- <*> objProp "preventDefault" o
  -- <*> objProp "stopPropagation" o
  -- <*> objProp "timeStamp" o
  <*> objProp "type" o

clipboardEventData o = commonEventData o ClipboardEvent <*> objProp "clipboardData" o


keyboardEventData o = commonEventData o KeyboardEvent
  <*> objProp "altKey" o
  <*> objProp "charCode" o
  <*> objProp "ctrlKey" o
  -- <*> objProp "getModifierState" o
  <*> objProp "key" o
  <*> objProp "keyCode" o
  <*> objProp "locale" o
  <*> objProp "location" o
  <*> objProp "metaKey" o
  <*> objProp "repeat" o
  <*> objProp "shiftKey" o
  <*> objProp "which" o

focusEventData o = commonEventData o FocusEvent <*> objProp "relatedTarget" o

formEventData o = commonEventData o FormEvent

mouseEventData o = commonEventData o MouseEvent
  <*> objProp "altKey" o
  <*> objProp "button" o
  <*> objProp "buttons" o
  <*> objProp "clientX" o
  <*> objProp "clientY" o
  <*> objProp "ctrlKey" o
  -- <*> objProp "getModifierState" o
  <*> objProp "metaKey" o
  <*> objProp "pageX" o
  <*> objProp "pageY" o
  <*> objProp "relatedTarget" o
  <*> objProp "screenX" o
  <*> objProp "screenY" o
  <*> objProp "shiftKey" o

instance FromJSRef MouseEvent where
  fromJSRef = runMaybeT . mouseEventData

touchEventData o = commonEventData o TouchEvent
  <*> objProp "altKey" o
  <*> objProp "changedTouches" o
  <*> objProp "ctrlKey" o
  -- <*> objProp "getModifierState" o
  <*> objProp "metaKey" o
  <*> objProp "shiftKey" o
  <*> objProp "targetTouches" o
  <*> objProp "touches" o

uiEventData o = commonEventData o UiEvent
  <*> objProp "detail" o
  <*> objProp "view" o

wheelEventData o = commonEventData o WheelEvent
  <*> objProp "deltaMode" o
  <*> objProp "deltaX" o
  <*> objProp "deltaY" o
  <*> objProp "deltaZ" o

