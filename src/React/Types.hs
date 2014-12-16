{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module React.Types where
import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.Reader
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import GHCJS.Types
import qualified Pipes.Safe as S

newtype ReactElement = Element (JSObject ())

data ReactNode = TextNode !Text
               | ElemNode !ReactElement

type DOMElement = DOM.Element
type ComponentContext = JSObject ()
type ReactT = S.SafeT
type ComponentT m = ReaderT ComponentContext m
type ComponentM = Reader ComponentContext
type Prop' a = Lens' (HashMap Text JSString) a
type Prop a = Prop' (Maybe a)

type Props = HashMap Text JSString
type State = HashMap Text JSString

type Element = Maybe Props -> [ReactNode] -> ReactElement

newtype Component = Component (JSRef Component)
newtype ComponentFactory st = ComponentFactory (JSFun (IO ReactElement))
data ComponentSpecification st = ComponentSpecification
  { componentSpecificationRender           :: ComponentT IO ReactElement
  , componentSpecificationDisplayName      :: Maybe JSString
  , componentSpecificationGetInitialState  :: Maybe (IO State)
  , componentSpecificationGetDefaultProps  :: Maybe (IO Props)
  -- , componentGetPropTypes 
  -- , componentMixins
  -- , componentStatics
  , componentSpecificationWillMount        :: Maybe (ComponentT IO ())
  , componentSpecificationDidMount         :: Maybe (ComponentT IO ())
  , componentSpecificationWillReceiveProps :: Maybe (JSRef st -> ComponentT IO ())
  , componentSpecificationShouldUpdate     :: Maybe (JSRef st -> JSRef st -> ComponentT IO Bool)
  -- TODO prevent setState in this context
  , componentSpecificationWillUpdate       :: Maybe (JSRef st -> JSRef st -> ComponentT IO ())
  , componentSpecificationDidUpdate        :: Maybe (JSRef st -> JSRef st -> ComponentT IO ())
  , componentSpecificationWillUnmount      :: Maybe (ComponentT IO ())
  }

makeFields ''ComponentSpecification


