{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module React.Props where
import Control.Lens
import Control.Monad
import Data.Monoid
import Prelude hiding (words, unwords)
import Data.Text (Text, singleton, words, unwords, split, intercalate)
import qualified Data.Text.Read as TR
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import GHCJS.Foreign
import GHCJS.Types
import React.Types (Prop', Prop, HandlerProp, ClipboardEvent, KeyboardEvent,
                    FocusEvent, FormEvent, MouseEvent, TouchEvent, UiEvent, WheelEvent)

present :: Lens' (Maybe JSString) Bool
present = lens getter setter
  where
    getter Nothing = False
    getter (Just _) = True

    setter _ False = Nothing
    setter _ True = Just ""

jsString :: (ToJSString a, FromJSString a) => Iso' a JSString
jsString = iso toJSString fromJSString

decimal :: Lens' (Maybe JSString) (Maybe Integer)
decimal = lens getter setter
  where
    getter Nothing  = Nothing
    getter (Just s) = case TR.decimal (fromJSString s) of
                        Left err -> Nothing
                        Right (n,_) -> Just n

    setter _ Nothing  = Nothing
    setter _ (Just n) = Just $ toJSString $ show n

spaceSeparated :: Lens' (Maybe JSString) [Text]
spaceSeparated = lens getter setter
  where
    getter Nothing      = []
    getter (Just s)     = words $ fromJSString s

    setter _ []         = Nothing
    setter _ xs@(_:_)   = Just $ toJSString $ unwords xs

styleMap :: Lens' (Maybe JSString) (HashMap Text Text)
styleMap = lens getter setter
  where
    getter Nothing      = H.empty
    getter (Just s)     = H.fromList $
                            map (\case (a:b:_) -> (a,b)) $
                            map (split (==':')) $
                            split (==';') $ fromJSString s

    setter :: Maybe JSString -> HashMap Text Text -> Maybe JSString
    setter _ h          = Just $ toJSString $
                            intercalate ";" $
                            map (\case (a,b) -> (a <> ":" <> b)) $
                            H.toList h
{-
data AcceptAttribute = Extension
                     | Mime
                     | Audio
                     | Video
                     | Image

accept :: [AcceptAttribute] -> Prop

-}

-- TODO: make it better than just Text
acceptCharset :: Prop JSString
acceptCharset = at "acceptCharset"

accessKey :: Prop Char
accessKey = at "accessKey" . lens getter setter
  where
    getter t = join $ fmap (fmap fst . (\x -> uncons (x :: Text)) . fromJSString) t
    setter _ = fmap (toJSString . singleton)

action :: Prop JSString
action = at "action"

allowFullScreen :: Prop' Bool
allowFullScreen = at "allowFullScreen" . present

allowTransparency :: Prop' Bool
allowTransparency = at "allowTransparency" . present

alt :: Prop JSString
alt = at "alt"

async :: Prop' Bool
async = at "async" . present

{-

-- "on" | "off"
autoComplete :: Prop Bool

autoPlay :: Prop Bool

cellPadding :: Prop Double

cellSpacing :: Prop Double

-- TODO something better than just Text
charSet :: Prop JSString

checked :: Prop Bool

classID :: Prop JSString
-}

className :: Prop' [Text]
className = at "className" . spaceSeparated

{-
cols :: Prop Int

colSpan :: Prop Int
content :: Prop JSString

-- "true" | "false"
contentEditable :: Prop Bool

-- TODO make CSS ID instead of Text
contextMenu :: Prop JSString

controls :: Prop Bool

coords :: Prop JSString

data CrossOrigin = Anonymous | UseCredentials

crossOrigin :: Prop CrossOrigin

-- TODO URI
data_ :: Prop JSString

-- TODO Make classy
dateTime :: Prop JSString

defer :: Prop Bool

data TextDirection = RightToLeft | LeftToRight | Auto

dir :: Prop TextDirection
-}

disabled :: Prop' Bool
disabled = at "disabled" . present

{-
download :: Prop Bool

draggable :: Prop Bool

encType :: Prop Text

form :: Prop Text

formNoValidate :: Prop Bool

-- "1" | "0"
frameBorder :: Prop Bool

-}
height :: Prop Integer
height = at "height" . decimal
{-

hidden :: Prop Bool

-}
-- TODO URI
href :: Prop JSString
href = at "href"

{-
hrefLang :: Prop Text

-- TODO CSS ID
htmlFor :: Prop Text

data Equiv = ContentType | DefaultStyle | Refresh

httpEquiv :: Prop Equiv

icon :: Prop Text
-}

id_ :: Prop JSString
id_ = at "id"

label :: Prop JSString
label = at "label"

{-
lang :: Prop Text

list :: Prop Text

loop :: Prop Bool

manifest :: Prop Text

max :: Prop Int

maxLength :: Prop Int

media :: Prop Text

mediaGroup :: Prop Text

data FormMethod = GET | POST

method :: Prop FormMethod

min :: Prop Int

multiple :: Prop Bool

muted :: Prop Bool

name :: Prop Text

noValidate :: Prop Bool

open :: Prop Bool

pattern :: Prop Text

placeholder :: Prop Text

-- TODO URI
poster :: Prop Text

data Preload = PreloadNone | PreloadMetadata | PreloadAuto

preload :: Prop Preload

radioGroup :: Prop Text

readOnly :: Prop Bool
-}
rel :: Prop JSString
rel = at "rel"

{-
required :: Prop Bool
-}

role :: Prop JSString
role = at "role"

{-
rows :: Prop Int

rowSpan :: Prop Int

data Sandbox = AllowSameOrigin | AllowTopNavigation | AllowForms | AllowPopups | AllowScripts | AllowPointerLock

sandbox :: Prop Sandbox

data Scope = Row | Col | RowGroup | ColGroup

scope :: Prop Scope

-- "yes" | "no"
scrolling :: Prop Bool

seamless :: Prop Bool

selected :: Prop Bool

shape :: Prop Text

size :: Prop Int

sizes :: Prop Text

span :: Prop Int

-- "true" | "false"
spellCheck :: Prop Text

src :: Prop Text
srcDoc :: Prop Text
srcSet :: Prop Text
start :: Prop Int
step :: Prop Int -}

style :: Prop' (HashMap Text Text)
style = at "style" . styleMap
{-
tabIndex :: Prop Int

data Target = Self | Blank | Parent | Top

target :: Prop Target

title :: Prop Text

type_ :: Prop Text

useMap :: Prop Text

value :: Prop Text
-}

width :: Prop Integer
width = at "width" . decimal

{-
data WMode = Transparent | Opaque

wmode :: Prop WMode

data AutoCapitalize = None | Sentences | Words | Characters

autoCapitalize :: Prop AutoCapitalize

-- "on" | "off"
autoCorrect :: Prop Bool

property :: Prop Text

itemProp :: Prop Text

itemScope :: Prop Text

itemType :: Prop Text
-}

dangerouslySetInnerHTML :: Prop JSString
dangerouslySetInnerHTML = at "dangerouslySetInnerHTML"

cx,cy :: Prop Integer
cx = prop "cx" . decimal
cy = prop "cy" . decimal

d :: Prop JSString
d = at "d"
{-dx
dy
-}
fill :: Prop JSString
fill = at "fill"
{-
fillOpacity
fontFamily
fontSize
fx
fy
gradientTransform
gradientUnits
markerEnd
markerMid
markerStart
offset
opacity
patternContentUnits
patternUnits
-}
points :: Prop JSString
points = at "points"
{-
preserveAspectRatio
-}
r :: Prop Integer
r = prop "r" . decimal

rx :: Prop Integer
rx = prop "rx" . decimal

ry :: Prop Integer
ry = prop "ry" . decimal
{-
spreadMethod
stopColor
stopOpacity
stroke
strokeDasharray
strokeLinecap
strokeOpacity
strokeWidth
textAnchor
-}
transform :: Prop JSString
transform = at "transform"
{-
version
viewBox
-}
x, x1, x2 :: Prop Integer
x  = prop "x"  . decimal
x1 = prop "x1" . decimal
x2 = prop "x2" . decimal

y, y1, y2 :: Prop Integer
y  = prop "y"  . decimal
y1 = prop "y1" . decimal
y2 = prop "y2" . decimal
{-
aria
-}

casted :: Functor f => Lens' (f (JSRef a)) (f (JSRef b))
casted = lens (fmap castRef) (\_ b -> fmap castRef b)
{-# INLINE casted #-}

prop :: Text -> Prop (JSRef a)
prop t = at t . casted

-- TODO Clean up all retained JSFuns in componentWillUnmount? Other places?
onCopy :: HandlerProp ClipboardEvent
onCopy = prop "onCopy" . casted

onCut :: HandlerProp ClipboardEvent
onCut = prop "onCut" . casted

onPaste :: HandlerProp ClipboardEvent
onPaste = prop "onPaste" . casted

onKeyDown :: HandlerProp KeyboardEvent
onKeyDown = prop "onKeyDown" . casted

onKeyPress :: HandlerProp KeyboardEvent
onKeyPress = prop "onKeyPress" . casted

onKeyUp :: HandlerProp KeyboardEvent
onKeyUp = prop "onKeyUp" . casted

onFocus :: HandlerProp FocusEvent
onFocus = prop "onFocus" . casted

onBlur :: HandlerProp FocusEvent
onBlur = prop "onBlur" . casted

onChange :: HandlerProp FormEvent
onChange = prop "onChange" . casted

onInput :: HandlerProp FormEvent
onInput = prop "onInput" . casted

onSubmit :: HandlerProp FormEvent
onSubmit = prop "onSubmit" . casted

onClick :: HandlerProp MouseEvent
onClick = prop "onClick" . casted

onDoubleClick :: HandlerProp MouseEvent
onDoubleClick = prop "onDoubleClick" . casted

onDrag :: HandlerProp MouseEvent
onDrag = prop "onDrag" . casted

onDragEnd :: HandlerProp MouseEvent
onDragEnd = prop "onDragEnd" . casted

onDragEnter :: HandlerProp MouseEvent
onDragEnter = prop "onDragEnter" . casted

onDragExit :: HandlerProp MouseEvent
onDragExit = prop "onDragExit" . casted

onDragLeave :: HandlerProp MouseEvent
onDragLeave = prop "onDragLeave" . casted

onDragOver :: HandlerProp MouseEvent
onDragOver = prop "onDragOver" . casted

onDragStart :: HandlerProp MouseEvent
onDragStart = prop "onDragStart" . casted

onDrop :: HandlerProp MouseEvent
onDrop = prop "onDrop" . casted

onMouseDown :: HandlerProp MouseEvent
onMouseDown = prop "onMouseDown" . casted

onMouseEnter :: HandlerProp MouseEvent
onMouseEnter = prop "onMouseEnter" . casted

onMouseLeave :: HandlerProp MouseEvent
onMouseLeave = prop "onMouseLeave" . casted

onMouseMove :: HandlerProp MouseEvent
onMouseMove = prop "onMouseMove" . casted

onMouseOut :: HandlerProp MouseEvent
onMouseOut = prop "onMouseOut" . casted

onMouseOver :: HandlerProp MouseEvent
onMouseOver = prop "onMouseOver" . casted

onMouseUp :: HandlerProp MouseEvent
onMouseUp = prop "onMouseUp" . casted

onTouchCancel :: HandlerProp TouchEvent
onTouchCancel = prop "onTouchCancel" . casted

onTouchEnd :: HandlerProp TouchEvent
onTouchEnd = prop "onTouchEnd" . casted

onTouchMove :: HandlerProp TouchEvent
onTouchMove = prop "onTouchMove" . casted

onTouchStart :: HandlerProp TouchEvent
onTouchStart = prop "onTouchStart" . casted

onScroll :: HandlerProp UiEvent
onScroll = prop "onScroll" . casted

onWheel :: HandlerProp WheelEvent
onWheel = prop "onWheel" . casted
