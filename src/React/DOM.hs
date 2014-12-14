{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module React.DOM where
import React
import Data.Text (Text)

class DOM a where
  dom :: Text -> a

instance DOM (Maybe Props -> [ReactNode] -> Either Text ReactElement) where
  dom = elem_

instance DOM Text where
  dom = id

instance DOM (Maybe Props -> [ReactNode] -> ReactElement) where
  dom = createElement

a_ :: DOM a => a
a_ = dom ("a" :: Text)

abbr_ :: DOM a => a
abbr_ = dom ("abbr" :: Text)

address_ :: DOM a => a
address_ = dom ("address" :: Text)

area_ :: DOM a => a
area_ = dom ("area" :: Text)

article_ :: DOM a => a
article_ = dom ("article" :: Text)

aside_ :: DOM a => a
aside_ = dom ("aside" :: Text)

audio_ :: DOM a => a
audio_ = dom ("audio" :: Text)

b_ :: DOM a => a
b_ = dom ("b" :: Text)

base_ :: DOM a => a
base_ = dom ("base" :: Text)

bdi_ :: DOM a => a
bdi_ = dom ("bdi" :: Text)

bdo_ :: DOM a => a
bdo_ = dom ("bdo" :: Text)

big_ :: DOM a => a
big_ = dom ("big" :: Text)

blockquote_ :: DOM a => a
blockquote_ = dom ("blockquote" :: Text)

body_ :: DOM a => a
body_ = dom ("body" :: Text)

br_ :: DOM a => a
br_ = dom ("br" :: Text)

button_ :: DOM a => a
button_ = dom ("button" :: Text)

canvas_ :: DOM a => a
canvas_ = dom ("canvas" :: Text)

caption_ :: DOM a => a
caption_ = dom ("caption" :: Text)

cite_ :: DOM a => a
cite_ = dom ("cite" :: Text)

code_ :: DOM a => a
code_ = dom ("code" :: Text)

col_ :: DOM a => a
col_ = dom ("col" :: Text)

colgroup_ :: DOM a => a
colgroup_ = dom ("colgroup" :: Text)

data_ :: DOM a => a
data_ = dom ("data" :: Text)

datalist_ :: DOM a => a
datalist_ = dom ("datalist" :: Text)

dd_ :: DOM a => a
dd_ = dom ("dd" :: Text)

del_ :: DOM a => a
del_ = dom ("del" :: Text)

details_ :: DOM a => a
details_ = dom ("details" :: Text)

dfn_ :: DOM a => a
dfn_ = dom ("dfn" :: Text)

dialog_ :: DOM a => a
dialog_ = dom ("dialog" :: Text)

div_ :: DOM a => a
div_ = dom ("div" :: Text)

dl_ :: DOM a => a
dl_ = dom ("dl" :: Text)

dt_ :: DOM a => a
dt_ = dom ("dt" :: Text)

em_ :: DOM a => a
em_ = dom ("em" :: Text)

embed_ :: DOM a => a
embed_ = dom ("embed" :: Text)

fieldset_ :: DOM a => a
fieldset_ = dom ("fieldset" :: Text)

figcaption_ :: DOM a => a
figcaption_ = dom ("figcaption" :: Text)

figure_ :: DOM a => a
figure_ = dom ("figure" :: Text)

footer_ :: DOM a => a
footer_ = dom ("footer" :: Text)

form_ :: DOM a => a
form_ = dom ("form" :: Text)

h1_ :: DOM a => a
h1_ = dom ("h1" :: Text)

h2_ :: DOM a => a
h2_ = dom ("h2" :: Text)

h3_ :: DOM a => a
h3_ = dom ("h3" :: Text)

h4_ :: DOM a => a
h4_ = dom ("h4" :: Text)

h5_ :: DOM a => a
h5_ = dom ("h5" :: Text)

h6_ :: DOM a => a
h6_ = dom ("h6" :: Text)

head_ :: DOM a => a
head_ = dom ("head" :: Text)

header_ :: DOM a => a
header_ = dom ("header" :: Text)

hr_ :: DOM a => a
hr_ = dom ("hr" :: Text)

html_ :: DOM a => a
html_ = dom ("html" :: Text)

i_ :: DOM a => a
i_ = dom ("i" :: Text)

iframe_ :: DOM a => a
iframe_ = dom ("iframe" :: Text)

img_ :: DOM a => a
img_ = dom ("img" :: Text)

input_ :: DOM a => a
input_ = dom ("input" :: Text)

ins_ :: DOM a => a
ins_ = dom ("ins" :: Text)

kbd_ :: DOM a => a
kbd_ = dom ("kbd" :: Text)

keygen_ :: DOM a => a
keygen_ = dom ("keygen" :: Text)

label_ :: DOM a => a
label_ = dom ("label" :: Text)

legend_ :: DOM a => a
legend_ = dom ("legend" :: Text)

li_ :: DOM a => a
li_ = dom ("li" :: Text)

link_ :: DOM a => a
link_ = dom ("link" :: Text)

main_ :: DOM a => a
main_ = dom ("main" :: Text)

map_ :: DOM a => a
map_ = dom ("map" :: Text)

mark_ :: DOM a => a
mark_ = dom ("mark" :: Text)

menu_ :: DOM a => a
menu_ = dom ("menu" :: Text)

menuitem_ :: DOM a => a
menuitem_ = dom ("menuitem" :: Text)

meta_ :: DOM a => a
meta_ = dom ("meta" :: Text)

meter_ :: DOM a => a
meter_ = dom ("meter" :: Text)

nav_ :: DOM a => a
nav_ = dom ("nav" :: Text)

noscript_ :: DOM a => a
noscript_ = dom ("noscript" :: Text)

object_ :: DOM a => a
object_ = dom ("object" :: Text)

ol_ :: DOM a => a
ol_ = dom ("ol" :: Text)

optgroup_ :: DOM a => a
optgroup_ = dom ("optgroup" :: Text)

option_ :: DOM a => a
option_ = dom ("option" :: Text)

output_ :: DOM a => a
output_ = dom ("output" :: Text)

p_ :: DOM a => a
p_ = dom ("p" :: Text)

param_ :: DOM a => a
param_ = dom ("param" :: Text)

picture_ :: DOM a => a
picture_ = dom ("picture" :: Text)

pre_ :: DOM a => a
pre_ = dom ("pre" :: Text)

progress_ :: DOM a => a
progress_ = dom ("progress" :: Text)

q_ :: DOM a => a
q_ = dom ("q" :: Text)

rp_ :: DOM a => a
rp_ = dom ("rp" :: Text)

rt_ :: DOM a => a
rt_ = dom ("rt" :: Text)

ruby_ :: DOM a => a
ruby_ = dom ("ruby" :: Text)

s_ :: DOM a => a
s_ = dom ("s" :: Text)

samp_ :: DOM a => a
samp_ = dom ("samp" :: Text)

script_ :: DOM a => a
script_ = dom ("script" :: Text)

section_ :: DOM a => a
section_ = dom ("section" :: Text)

select_ :: DOM a => a
select_ = dom ("select" :: Text)

small_ :: DOM a => a
small_ = dom ("small" :: Text)

source_ :: DOM a => a
source_ = dom ("source" :: Text)

span_ :: DOM a => a
span_ = dom ("span" :: Text)

strong_ :: DOM a => a
strong_ = dom ("strong" :: Text)

style_ :: DOM a => a
style_ = dom ("style" :: Text)

sub_ :: DOM a => a
sub_ = dom ("sub" :: Text)

summary_ :: DOM a => a
summary_ = dom ("summary" :: Text)

sup_ :: DOM a => a
sup_ = dom ("sup" :: Text)

table_ :: DOM a => a
table_ = dom ("table" :: Text)

tbody_ :: DOM a => a
tbody_ = dom ("tbody" :: Text)

td_ :: DOM a => a
td_ = dom ("td" :: Text)

textarea_ :: DOM a => a
textarea_ = dom ("textarea" :: Text)

tfoot_ :: DOM a => a
tfoot_ = dom ("tfoot" :: Text)

th_ :: DOM a => a
th_ = dom ("th" :: Text)

thead_ :: DOM a => a
thead_ = dom ("thead" :: Text)

time_ :: DOM a => a
time_ = dom ("time" :: Text)

title_ :: DOM a => a
title_ = dom ("title" :: Text)

tr_ :: DOM a => a
tr_ = dom ("tr" :: Text)

track_ :: DOM a => a
track_ = dom ("track" :: Text)

u_ :: DOM a => a
u_ = dom ("u" :: Text)

ul_ :: DOM a => a
ul_ = dom ("ul" :: Text)

var_ :: DOM a => a
var_ = dom ("var" :: Text)

video_ :: DOM a => a
video_ = dom ("video" :: Text)

wbr_ :: DOM a => a
wbr_ = dom ("wbr" :: Text)

circle_ :: DOM a => a
circle_ = dom ("circle" :: Text)

defs_ :: DOM a => a
defs_ = dom ("defs" :: Text)

ellipse_ :: DOM a => a
ellipse_ = dom ("ellipse" :: Text)

g_ :: DOM a => a
g_ = dom ("g" :: Text)

line_ :: DOM a => a
line_ = dom ("line" :: Text)

linearGradient_ :: DOM a => a
linearGradient_ = dom ("linearGradient" :: Text)

mask_ :: DOM a => a
mask_ = dom ("mask" :: Text)

path_ :: DOM a => a
path_ = dom ("path" :: Text)

pattern_ :: DOM a => a
pattern_ = dom ("pattern" :: Text)

polygon_ :: DOM a => a
polygon_ = dom ("polygon" :: Text)

polyline_ :: DOM a => a
polyline_ = dom ("polyline" :: Text)

radialGradient_ :: DOM a => a
radialGradient_ = dom ("radialGradient" :: Text)

rect_ :: DOM a => a
rect_ = dom ("rect" :: Text)

stop_ :: DOM a => a
stop_ = dom ("stop" :: Text)

svg_ :: DOM a => a
svg_ = dom ("svg" :: Text)

text_ :: DOM a => a
text_ = dom ("text" :: Text)

tspan_ :: DOM a => a
tspan_ = dom ("tspan" :: Text)
