{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module React.DOM where
import React.Internal
import React.Types
import Data.Text (Text)

a_ :: Element
a_ = createElement ("a" :: Text)

abbr_ :: Element
abbr_ = createElement ("abbr" :: Text)

address_ :: Element
address_ = createElement ("address" :: Text)

area_ :: Element
area_ = createElement ("area" :: Text)

article_ :: Element
article_ = createElement ("article" :: Text)

aside_ :: Element
aside_ = createElement ("aside" :: Text)

audio_ :: Element
audio_ = createElement ("audio" :: Text)

b_ :: Element
b_ = createElement ("b" :: Text)

base_ :: Element
base_ = createElement ("base" :: Text)

bdi_ :: Element
bdi_ = createElement ("bdi" :: Text)

bdo_ :: Element
bdo_ = createElement ("bdo" :: Text)

big_ :: Element
big_ = createElement ("big" :: Text)

blockquote_ :: Element
blockquote_ = createElement ("blockquote" :: Text)

body_ :: Element
body_ = createElement ("body" :: Text)

br_ :: Element
br_ = createElement ("br" :: Text)

button_ :: Element
button_ = createElement ("button" :: Text)

canvas_ :: Element
canvas_ = createElement ("canvas" :: Text)

caption_ :: Element
caption_ = createElement ("caption" :: Text)

cite_ :: Element
cite_ = createElement ("cite" :: Text)

code_ :: Element
code_ = createElement ("code" :: Text)

col_ :: Element
col_ = createElement ("col" :: Text)

colgroup_ :: Element
colgroup_ = createElement ("colgroup" :: Text)

data_ :: Element
data_ = createElement ("data" :: Text)

datalist_ :: Element
datalist_ = createElement ("datalist" :: Text)

dd_ :: Element
dd_ = createElement ("dd" :: Text)

del_ :: Element
del_ = createElement ("del" :: Text)

details_ :: Element
details_ = createElement ("details" :: Text)

dfn_ :: Element
dfn_ = createElement ("dfn" :: Text)

dialog_ :: Element
dialog_ = createElement ("dialog" :: Text)

div_ :: Element
div_ = createElement ("div" :: Text)

dl_ :: Element
dl_ = createElement ("dl" :: Text)

dt_ :: Element
dt_ = createElement ("dt" :: Text)

em_ :: Element
em_ = createElement ("em" :: Text)

embed_ :: Element
embed_ = createElement ("embed" :: Text)

fieldset_ :: Element
fieldset_ = createElement ("fieldset" :: Text)

figcaption_ :: Element
figcaption_ = createElement ("figcaption" :: Text)

figure_ :: Element
figure_ = createElement ("figure" :: Text)

footer_ :: Element
footer_ = createElement ("footer" :: Text)

form_ :: Element
form_ = createElement ("form" :: Text)

h1_ :: Element
h1_ = createElement ("h1" :: Text)

h2_ :: Element
h2_ = createElement ("h2" :: Text)

h3_ :: Element
h3_ = createElement ("h3" :: Text)

h4_ :: Element
h4_ = createElement ("h4" :: Text)

h5_ :: Element
h5_ = createElement ("h5" :: Text)

h6_ :: Element
h6_ = createElement ("h6" :: Text)

head_ :: Element
head_ = createElement ("head" :: Text)

header_ :: Element
header_ = createElement ("header" :: Text)

hr_ :: Element
hr_ = createElement ("hr" :: Text)

html_ :: Element
html_ = createElement ("html" :: Text)

i_ :: Element
i_ = createElement ("i" :: Text)

iframe_ :: Element
iframe_ = createElement ("iframe" :: Text)

img_ :: Element
img_ = createElement ("img" :: Text)

input_ :: Element
input_ = createElement ("input" :: Text)

ins_ :: Element
ins_ = createElement ("ins" :: Text)

kbd_ :: Element
kbd_ = createElement ("kbd" :: Text)

keygen_ :: Element
keygen_ = createElement ("keygen" :: Text)

label_ :: Element
label_ = createElement ("label" :: Text)

legend_ :: Element
legend_ = createElement ("legend" :: Text)

li_ :: Element
li_ = createElement ("li" :: Text)

link_ :: Element
link_ = createElement ("link" :: Text)

main_ :: Element
main_ = createElement ("main" :: Text)

map_ :: Element
map_ = createElement ("map" :: Text)

mark_ :: Element
mark_ = createElement ("mark" :: Text)

menu_ :: Element
menu_ = createElement ("menu" :: Text)

menuitem_ :: Element
menuitem_ = createElement ("menuitem" :: Text)

meta_ :: Element
meta_ = createElement ("meta" :: Text)

meter_ :: Element
meter_ = createElement ("meter" :: Text)

nav_ :: Element
nav_ = createElement ("nav" :: Text)

noscript_ :: Element
noscript_ = createElement ("noscript" :: Text)

object_ :: Element
object_ = createElement ("object" :: Text)

ol_ :: Element
ol_ = createElement ("ol" :: Text)

optgroup_ :: Element
optgroup_ = createElement ("optgroup" :: Text)

option_ :: Element
option_ = createElement ("option" :: Text)

output_ :: Element
output_ = createElement ("output" :: Text)

p_ :: Element
p_ = createElement ("p" :: Text)

param_ :: Element
param_ = createElement ("param" :: Text)

picture_ :: Element
picture_ = createElement ("picture" :: Text)

pre_ :: Element
pre_ = createElement ("pre" :: Text)

progress_ :: Element
progress_ = createElement ("progress" :: Text)

q_ :: Element
q_ = createElement ("q" :: Text)

rp_ :: Element
rp_ = createElement ("rp" :: Text)

rt_ :: Element
rt_ = createElement ("rt" :: Text)

ruby_ :: Element
ruby_ = createElement ("ruby" :: Text)

s_ :: Element
s_ = createElement ("s" :: Text)

samp_ :: Element
samp_ = createElement ("samp" :: Text)

script_ :: Element
script_ = createElement ("script" :: Text)

section_ :: Element
section_ = createElement ("section" :: Text)

select_ :: Element
select_ = createElement ("select" :: Text)

small_ :: Element
small_ = createElement ("small" :: Text)

source_ :: Element
source_ = createElement ("source" :: Text)

span_ :: Element
span_ = createElement ("span" :: Text)

strong_ :: Element
strong_ = createElement ("strong" :: Text)

style_ :: Element
style_ = createElement ("style" :: Text)

sub_ :: Element
sub_ = createElement ("sub" :: Text)

summary_ :: Element
summary_ = createElement ("summary" :: Text)

sup_ :: Element
sup_ = createElement ("sup" :: Text)

table_ :: Element
table_ = createElement ("table" :: Text)

tbody_ :: Element
tbody_ = createElement ("tbody" :: Text)

td_ :: Element
td_ = createElement ("td" :: Text)

textarea_ :: Element
textarea_ = createElement ("textarea" :: Text)

tfoot_ :: Element
tfoot_ = createElement ("tfoot" :: Text)

th_ :: Element
th_ = createElement ("th" :: Text)

thead_ :: Element
thead_ = createElement ("thead" :: Text)

time_ :: Element
time_ = createElement ("time" :: Text)

title_ :: Element
title_ = createElement ("title" :: Text)

tr_ :: Element
tr_ = createElement ("tr" :: Text)

track_ :: Element
track_ = createElement ("track" :: Text)

u_ :: Element
u_ = createElement ("u" :: Text)

ul_ :: Element
ul_ = createElement ("ul" :: Text)

var_ :: Element
var_ = createElement ("var" :: Text)

video_ :: Element
video_ = createElement ("video" :: Text)

wbr_ :: Element
wbr_ = createElement ("wbr" :: Text)

circle_ :: Element
circle_ = createElement ("circle" :: Text)

defs_ :: Element
defs_ = createElement ("defs" :: Text)

ellipse_ :: Element
ellipse_ = createElement ("ellipse" :: Text)

g_ :: Element
g_ = createElement ("g" :: Text)

line_ :: Element
line_ = createElement ("line" :: Text)

linearGradient_ :: Element
linearGradient_ = createElement ("linearGradient" :: Text)

mask_ :: Element
mask_ = createElement ("mask" :: Text)

path_ :: Element
path_ = createElement ("path" :: Text)

pattern_ :: Element
pattern_ = createElement ("pattern" :: Text)

polygon_ :: Element
polygon_ = createElement ("polygon" :: Text)

polyline_ :: Element
polyline_ = createElement ("polyline" :: Text)

radialGradient_ :: Element
radialGradient_ = createElement ("radialGradient" :: Text)

rect_ :: Element
rect_ = createElement ("rect" :: Text)

stop_ :: Element
stop_ = createElement ("stop" :: Text)

svg_ :: Element
svg_ = createElement ("svg" :: Text)

text_ :: Element
text_ = createElement ("text" :: Text)

tspan_ :: Element
tspan_ = createElement ("tspan" :: Text)
