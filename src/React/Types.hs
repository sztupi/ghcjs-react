{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
module React.Types where
import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.Reader
import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Data.Time
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import GHCJS.Types
import qualified Pipes.Safe as S

newtype ReactElement = Element (JSObject ())

data ReactNode = TextNode !Text
               | ElemNode !ReactElement

instance IsString ReactNode where
  fromString = TextNode . fromString

type DOMElement = DOM.Element
type DOMEventTarget = DOM.EventTarget
type DOMEvent = DOM.Event
type DOMTouchList = JSRef ()
type DOMAbstractView = JSRef ()
type DOMDataTransfer = JSRef ()

type ComponentContext = JSObject ()
type ReactT = S.SafeT
type ComponentT m = ReaderT ComponentContext m
type ComponentM = Reader ComponentContext
type Prop' a = Lens' (HashMap Text JSString) a
type Prop a = Prop' (Maybe a)
type HandlerProp a = Prop (JSFun (a -> IO ()))

type Props = HashMap Text JSString
type State = HashMap Text JSString

type Element = Maybe Props -> [ReactNode] -> ReactElement

newtype Component = Component (JSRef Component)
newtype ComponentFactory (m :: * -> *) st = ComponentFactory (JSFun (IO ReactElement))

data ComponentSpecification m ps st = ComponentSpecification
  { componentSpecificationRender           :: ComponentT m ReactElement
  , componentSpecificationDisplayName      :: Maybe JSString
  , componentSpecificationGetInitialState  :: Maybe (ComponentT m (JSRef st))
  , componentSpecificationGetDefaultProps  :: Maybe (ComponentT m (JSRef ps))
  -- , componentGetPropTypes 
  -- , componentMixins
  -- , componentStatics
  , componentSpecificationWillMount        :: Maybe (ComponentT m ())
  , componentSpecificationDidMount         :: Maybe (ComponentT m ())
  , componentSpecificationWillReceiveProps :: Maybe (JSRef ps -> ComponentT m ())
  , componentSpecificationShouldUpdate     :: Maybe (JSRef ps -> JSRef st -> ComponentT m Bool)
  -- TODO prevent setState in this context
  , componentSpecificationWillUpdate       :: Maybe (JSRef ps -> JSRef st -> ComponentT m ())
  , componentSpecificationDidUpdate        :: Maybe (JSRef ps -> JSRef st -> ComponentT m ())
  , componentSpecificationWillUnmount      :: Maybe (ComponentT m ())
  }

makeFields ''ComponentSpecification

data ClipboardEvent = ClipboardEvent
  { clipboardEventBubbles          :: Bool
  , clipboardEventCancelable       :: Bool
  , clipboardEventCurrentTarget    :: DOMEventTarget
  , clipboardEventDefaultPrevented :: Bool
  , clipboardEventEventPhase       :: Int
  , clipboardEventIsTrusted        :: Bool
  , clipboardEventNativeEvent      :: DOMEvent
  -- , clipboardEventPreventDefault   :: IO ()
  -- , clipboardEventStopPropagation  :: IO ()
  -- , clipboardEventTimeStamp        :: UTCTime
  , clipboardEventType_            :: Text
  , clipboardEventClipboardData    :: DOMDataTransfer
  }

makeFields ''ClipboardEvent

data KeyboardEvent = KeyboardEvent
  { keyboardEventBubbles          :: Bool
  , keyboardEventCancelable       :: Bool
  , keyboardEventCurrentTarget    :: DOMEventTarget
  , keyboardEventDefaultPrevented :: Bool
  , keyboardEventEventPhase       :: Int
  , keyboardEventIsTrusted        :: Bool
  , keyboardEventNativeEvent      :: DOMEvent
  -- , keyboardEventPreventDefault   :: IO ()
  -- , keyboardEventStopPropagation  :: IO ()
  -- , keyboardEventTimeStamp        :: UTCTime
  , keyboardEventType_            :: Text
  , keyboardEventAltKey           :: Bool
  , keyboardEventCharCode         :: Int
  , keyboardEventCtrlKey          :: Bool
  -- , keyboardEventGetModifierState :: () -> IO ()
  , keyboardEventKey              :: Text
  , keyboardEventKeyCode          :: Int
  , keyboardEventLocale           :: Text
  , keyboardEventLocation         :: Int
  , keyboardEventMetaKey          :: Bool
  , keyboardEventRepeat           :: Bool
  , keyboardEventShiftKey         :: Bool
  , keyboardEventWhich            :: Int
  }

makeFields ''KeyboardEvent

data FocusEvent = FocusEvent
  { focusEventBubbles          :: Bool
  , focusEventCancelable       :: Bool
  , focusEventCurrentTarget    :: DOMEventTarget
  , focusEventDefaultPrevented :: Bool
  , focusEventEventPhase       :: Int
  , focusEventIsTrusted        :: Bool
  , focusEventNativeEvent      :: DOMEvent
  -- , focusEventPreventDefault   :: IO ()
  -- , focusEventStopPropagation  :: IO ()
  -- , focusEventTimeStamp        :: UTCTime
  , focusEventType_            :: Text
  , focusEventRelatedTarget    :: DOMEventTarget
  }

makeFields ''FocusEvent

data FormEvent = FormEvent
  { formEventBubbles          :: Bool
  , formEventCancelable       :: Bool
  , formEventCurrentTarget    :: DOMEventTarget
  , formEventDefaultPrevented :: Bool
  , formEventEventPhase       :: Int
  , formEventIsTrusted        :: Bool
  , formEventNativeEvent      :: DOMEvent
  -- , formEventPreventDefault   :: IO ()
  -- , formEventStopPropagation  :: IO ()
  -- , formEventTimeStamp        :: UTCTime
  , formEventType_            :: Text
  }

makeFields ''FormEvent

data MouseEvent = MouseEvent
  { mouseEventBubbles          :: Bool
  , mouseEventCancelable       :: Bool
  , mouseEventCurrentTarget    :: DOMEventTarget
  , mouseEventDefaultPrevented :: Bool
  , mouseEventEventPhase       :: Int
  , mouseEventIsTrusted        :: Bool
  , mouseEventNativeEvent      :: DOMEvent
  -- , mouseEventPreventDefault   :: IO ()
  -- , mouseEventStopPropagation  :: IO ()
  -- , mouseEventTimeStamp        :: UTCTime
  , mouseEventType_            :: Text
  , mouseEventAltKey           :: Bool
  , mouseEventButton           :: Int
  , mouseEventButtons          :: Int
  , mouseEventClientX          :: Double
  , mouseEventClientY          :: Double
  , mouseEventCtrlKey          :: Bool
  -- , mouseEventGetModifierState :: () -> IO ()
  , mouseEventMetaKey          :: Bool
  , mouseEventPageX            :: Double
  , mouseEventPageY            :: Double
  , mouseEventRelatedTarget    :: DOMEventTarget
  , mouseEventScreenX          :: Double
  , mouseEventScreenY          :: Double
  , mouseEventShiftKey         :: Bool
  }

makeFields ''MouseEvent

data TouchEvent = TouchEvent
  { touchEventBubbles          :: Bool
  , touchEventCancelable       :: Bool
  , touchEventCurrentTarget    :: DOMEventTarget
  , touchEventDefaultPrevented :: Bool
  , touchEventEventPhase       :: Int
  , touchEventIsTrusted        :: Bool
  , touchEventNativeEvent      :: DOMEvent
  -- , touchEventPreventDefault   :: IO ()
  -- , touchEventStopPropagation  :: IO ()
  -- , touchEventTimeStamp        :: UTCTime
  , touchEventType_            :: Text
  , touchEventAltKey           :: Bool
  , touchEventChangedTouches   :: DOMTouchList
  , touchEventCtrlKey          :: Bool
  -- , touchEventGetModifierState :: () -> IO ()
  , touchEventMetaKey          :: Bool
  , touchEventShiftKey         :: Bool
  , touchEventTargetTouches    :: DOMTouchList
  , touchEventTouches          :: DOMTouchList
  }

makeFields ''TouchEvent

data UiEvent = UiEvent
  { uiEventBubbles          :: Bool
  , uiEventCancelable       :: Bool
  , uiEventCurrentTarget    :: DOMEventTarget
  , uiEventDefaultPrevented :: Bool
  , uiEventEventPhase       :: Int
  , uiEventIsTrusted        :: Bool
  , uiEventNativeEvent      :: DOMEvent
  -- , uiEventPreventDefault   :: IO ()
  -- , uiEventStopPropagation  :: IO ()
  -- , uiEventTimeStamp        :: UTCTime
  , uiEventType_            :: Text
  , uiEventDetail           :: Double
  , uiEventView             :: DOMAbstractView
  }

makeFields ''UiEvent

data WheelEvent = WheelEvent
  { wheelEventBubbles          :: Bool
  , wheelEventCancelable       :: Bool
  , wheelEventCurrentTarget    :: DOMEventTarget
  , wheelEventDefaultPrevented :: Bool
  , wheelEventEventPhase       :: Int
  , wheelEventIsTrusted        :: Bool
  , wheelEventNativeEvent      :: DOMEvent
  -- , wheelEventPreventDefault   :: IO ()
  -- , wheelEventStopPropagation  :: IO ()
  -- , wheelEventTimeStamp        :: UTCTime
  , wheelEventType_            :: Text
  , wheelEventDeltaMode        :: Double
  , wheelEventDeltaX           :: Double
  , wheelEventDeltaY           :: Double
  , wheelEventDeltaZ           :: Double
  }

makeFields ''WheelEvent

