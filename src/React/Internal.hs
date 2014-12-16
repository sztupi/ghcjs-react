{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
module React.Internal where
import Data.HashMap.Strict (HashMap, toList)
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Types
import React.Raw
import React.Types
import System.IO.Unsafe

mkNodeRef :: ReactNode -> JSRef a
mkNodeRef (ElemNode (Element ref)) = castRef ref
mkNodeRef (TextNode t) = castRef $ toJSString t

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


