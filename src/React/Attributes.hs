{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
module React.Attributes where
import Control.Lens
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)

type Prop a = Lens' (HashMap Text Text) (Maybe a)

{-
data AcceptAttribute = Extension
                     | Mime
                     | Audio
                     | Video
                     | Image

accept :: [AcceptAttribute] -> Prop

-- TODO: make it better than just Text
acceptCharset :: Prop Text

accessKey :: Prop Char

action :: Prop Text

allowFullScreen :: Prop Bool

allowTransparency :: Prop Bool

alt :: Prop Text

async :: Prop Bool

-- "on" | "off"
autoComplete :: Prop Bool

autoPlay :: Prop Bool

cellPadding :: Prop Double

cellSpacing :: Prop Double

-- TODO something better than just Text
charSet :: Prop Text

checked :: Prop Bool

classID :: Prop Text
-}

className :: Prop Text
className = at "className"

{-
cols :: Prop Int

colSpan :: Prop Int
content :: Prop Text

-- "true" | "false"
contentEditable :: Prop Bool

-- TODO make CSS ID instead of Text
contextMenu :: Prop Text

controls :: Prop Bool

coords :: Prop Text

data CrossOrigin = Anonymous | UseCredentials

crossOrigin :: Prop CrossOrigin

-- TODO URI
data_ :: Prop Text

-- TODO Make classy
dateTime :: Prop Text

defer :: Prop Bool

data TextDirection = RightToLeft | LeftToRight | Auto

dir :: Prop TextDirection

disabled :: Prop Bool

download :: Prop Bool

draggable :: Prop Bool

encType :: Prop Text

form :: Prop Text

formNoValidate :: Prop Bool

-- "1" | "0"
frameBorder :: Prop Bool

height :: Prop Int

hidden :: Prop Bool

-}
-- TODO URI
href :: Prop Text
href = at "href"

{-
hrefLang :: Prop Text

-- TODO CSS ID
htmlFor :: Prop Text

data Equiv = ContentType | DefaultStyle | Refresh

httpEquiv :: Prop Equiv

icon :: Prop Text
-}

id_ :: Prop Text
id_ = at "id"

label :: Prop Text
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
rel :: Prop Text
rel = at "rel"

{-
required :: Prop Bool

role :: Prop Text

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
step :: Prop Int
style :: Prop (HashMap Text Text)
tabIndex :: Prop Int

data Target = Self | Blank | Parent | Top

target :: Prop Target

title :: Prop Text

type_ :: Prop Text

useMap :: Prop Text

value :: Prop Text

width :: Prop Int

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

dangerouslySetInnerHTML :: Prop Text
-}

{-
cx
cy
d
dx
dy
fill
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
points
preserveAspectRatio
r
rx
ry
spreadMethod
stopColor
stopOpacity
stroke
strokeDasharray
strokeLinecap
strokeOpacity
strokeWidth
textAnchor
transform
version
viewBox
x1
x2
x
y1
y2
y
aria
prop
-}
