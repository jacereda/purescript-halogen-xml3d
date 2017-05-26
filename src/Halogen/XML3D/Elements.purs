module Halogen.XML3D.Elements where

import Prelude
import DOM.XML3D.Indexed as I
import DOM.XML3D.Indexed.Vec2 (Vec2, renderVec2)
import DOM.XML3D.Indexed.Vec3 (Vec3, renderVec3)
import Halogen.HTML.Core (ElemName(..), HTML, text)
import Halogen.HTML.Elements (Node, element)
import Halogen.HTML.Properties (IProp)


-- Root element

xml3d :: forall p i. Node I.XML3Dxml3d p i
xml3d = element (ElemName "xml3d")

-- Scene elements

group :: forall p i. Node I.XML3Dgroup p i
group = element (ElemName "group")

view :: forall p i. Node I.XML3Dview p i
view = element (ElemName "view")

light :: forall p i. Node I.XML3Dlight p i
light = element (ElemName "light")

mesh :: forall p i. Node I.XML3Dmesh p i
mesh = element (ElemName "mesh")

model :: forall p i. Node I.XML3Dmodel p i
model = element (ElemName "model")

-- Definition areas

defs :: forall p i. Node I.XML3Ddefs p i
defs = element (ElemName "defs")

-- Property elements

material :: forall p i. Node I.XML3Dmaterial p i
material = element (ElemName "material")

transform :: forall p i. Node I.XML3Dtransform p i
transform = element (ElemName "transform")

-- Assets elements

asset :: forall p i. Node I.XML3Dasset p i
asset = element (ElemName "asset")

assetdata :: forall p i. Node I.XML3Dassetdata p i
assetdata = element (ElemName "assetdata")

assetmesh :: forall p i. Node I.XML3Dassetmesh p i
assetmesh = element (ElemName "assetmesh")

-- Data and dataflow elements

_data :: forall p i. Node I.XML3Ddata p i
_data = element (ElemName "data")

dataflow :: forall p i. Node I.XML3Ddataflow p i
dataflow = element (ElemName "dataflow")

float :: forall p i. Array (IProp I.XML3Dfloat i) -> Array Number -> HTML p i
float p vs = element (ElemName "float") p $ text <<< show <$> vs

float2 :: forall p i. Array (IProp I.XML3Dfloat2 i) -> Array Vec2 -> HTML p i
float2 p vs = element (ElemName "float2") p $ text <<< renderVec2 <$> vs

float3 :: forall p i. Array (IProp I.XML3Dfloat3 i) -> Array Vec3 -> HTML p i
float3 p vs = element (ElemName "float3") p $ text <<< renderVec3 <$> vs

int :: forall p i. Node I.XML3Dint p i
int = element (ElemName "int")

int4 :: forall p i. Node I.XML3Dint4 p i
int4 = element (ElemName "int4")

bool :: forall p i. Node I.XML3Dbool p i
bool = element (ElemName "bool")

string :: forall p i. Node I.XML3Dbool p i
string = element (ElemName "string")

texture :: forall p i. Node I.XML3Dtexture p i
texture = element (ElemName "texture")

