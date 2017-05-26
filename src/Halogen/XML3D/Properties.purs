module Halogen.XML3D.Properties where

import Prelude
import DOM.HTML.Indexed (CSSPixel)
import DOM.XML3D.Indexed.AxisAngle (AxisAngle, renderAxisAngle)
import DOM.XML3D.Indexed.Light (LightModel, renderLightModel)
import DOM.XML3D.Indexed.Material (MatModel, renderMatModel)
import DOM.XML3D.Indexed.Mesh (MeshType, renderMeshType)
import DOM.XML3D.Indexed.Texture (Filter, Wrap, renderFilter, renderWrap)
import DOM.XML3D.Indexed.Vec3 (Vec3, renderVec3)
import DOM.XML3D.Indexed.View (ViewModel, renderViewModel)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties (IProp, attr)

width :: forall r i. CSSPixel -> IProp (width :: CSSPixel | r) i
width = attr (AttrName "width") <<< show

height :: forall r i. CSSPixel -> IProp (height :: CSSPixel | r) i
height = attr (AttrName "height") <<< show

src :: forall r i. String -> IProp (src :: String | r) i
src = attr (AttrName "src")

id :: forall r i. String -> IProp (id :: String | r) i
id = attr (AttrName "id")

name :: forall r i. String -> IProp (name :: String | r) i
name = attr (AttrName "name")

material :: forall r i. String -> IProp (material :: String | r) i
material = attr (AttrName "material")

transform :: forall r i. String -> IProp (transform :: String | r) i
transform = attr (AttrName "transform")

viewModel :: forall r i. ViewModel -> IProp (model :: ViewModel | r) i
viewModel = attr (AttrName "model") <<< renderViewModel

matModel :: forall r i. MatModel -> IProp (model :: MatModel | r) i
matModel = attr (AttrName "model") <<< renderMatModel

lightModel :: forall r i. LightModel -> IProp (model :: LightModel | r) i
lightModel = attr (AttrName "model") <<< renderLightModel

translation :: forall r i. Vec3 -> IProp (translation :: Vec3 | r) i
translation = attr (AttrName "translation") <<< renderVec3

rotation :: forall r i. AxisAngle -> IProp (rotation :: AxisAngle | r) i
rotation = attr (AttrName "rotation") <<< renderAxisAngle

scale :: forall r i. Vec3 -> IProp (scale :: Vec3 | r) i
scale = attr (AttrName "scale") <<< renderVec3

_type :: forall r i. MeshType -> IProp (type :: MeshType | r) i
_type = attr (AttrName "type") <<< renderMeshType

filter :: forall r i. Filter -> IProp (filter :: Filter | r) i
filter = attr (AttrName "filter") <<< renderFilter

wrap :: forall r i. Wrap -> IProp (wrap :: Wrap | r) i
wrap = attr (AttrName "wrap") <<< renderWrap
