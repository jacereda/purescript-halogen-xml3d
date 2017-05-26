module Example where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.XML3D.Elements as XH
import Halogen.XML3D.Events as XE
import Halogen.XML3D.Properties as XP
import DOM.Event.MouseEvent (MouseEvent, clientX, clientY)
import DOM.XML3D.Event.Types (FrameDrawnEvent)
import DOM.XML3D.Indexed.AxisAngle (AxisAngle(..))
import DOM.XML3D.Indexed.Light (LightModel(..))
import DOM.XML3D.Indexed.Material (MatModel(..))
import DOM.XML3D.Indexed.Texture (Filter(..), MagFilter(..), MinFilter(..), Wrap(..), WrapMode(..))
import DOM.XML3D.Indexed.Vec3 (Vec3(..))
import DOM.XML3D.Indexed.View (ViewModel(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

data State = State Int Int

data Query a = Move MouseEvent a
             | Drawn FrameDrawnEvent a

data Message = Moved Number

example :: forall m. H.Component HH.HTML Query Unit Message m
example =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = State 0 0

  render :: State -> H.ComponentHTML Query
  render (State x y) =
    HH.div [ HP.class_ $ HH.ClassName "_xml3d_hideDiv"
           , HE.onMouseMove (HE.input Move)
           ]
    [ XH.xml3d [ XP.width 800
               , XP.height 600
               , XE.onFrameDrawn (HE.input Drawn)
               ]
      [ XH.defs []        
        [ XH.material [ XP.id "mouseMat"
                      , XP.matModel Phong
                      ]
          [ XH.float3 [ XP.name "diffuseColor" ] [Vec3 r g b]
          , XH.float [ XP.name "ambientIntensity" ] [0.5]
          ]
        , XH.material [ XP.id "videoMat"
                      , XP.matModel Phong
                      ]
          [ XH.float3 [ XP.name "diffuseColor" ] [Vec3 1.0 1.0 1.0]
          , XH.texture [ XP.name "diffuseTexture"
                       , XP.filter $ Filter MinLinear MagLinear
                       , XP.wrap $ Wrap Clamp Clamp
                       ]
            [ HH.video [ HP.src $ base <> "../resources/videos/nescafe.mp4"
                       , HP.autoplay true
                       , HP.loop true
                       , HP.muted true
                       ] []
            ]
          ]
        , XH.transform [ XP.id "cameraTransform"
                       , XP.translation $ Vec3 0.0 0.0 250.0
                       ] []
        , XH.transform [ XP.id "objectTransform"
                       , XP.translation $ Vec3 (-50.0) (-50.0) 0.0
                       ] []
        , XH.transform [ XP.id "engineerTransform"
                       , XP.translation $ Vec3 50.0 (-50.0) 0.0
                       ] []
        , XH.transform [ XP.id "planeTransform"
                       , XP.translation $ Vec3 (-50.0) 50.0 0.0
                       , XP.scale $ Vec3 10.0 10.0 10.0
                       ] []
        , XH.transform [ XP.id "cubeTransform"
                       , XP.translation $ Vec3 50.0 50.0 0.0
                       , XP.scale $ Vec3 15.0 15.0 15.0
                       ] []
        , XH.transform [ XP.id "pitchTransform" 
                       , XP.rotation $ AxisAngle 1.0 0.0 0.0 (fy / 50.0) ] []
        , XH.transform [ XP.id "headingTransform" 
                       , XP.rotation $ AxisAngle 0.0 1.0 0.0 (fx / 50.0) ] []
        , XH.light [ XP.lightModel Directional ]
          [ XH.float3 [ XP.name "intensity" ] [ Vec3 1.0 1.0 1.0 ] ]
        ]
      , XH.view [ XP.transform "#cameraTransform"
                , XP.viewModel Perspective
                ]
        []
      , XH.group [ XP.material "#mouseMat"
                 , XP.transform "#objectTransform"
                 ] $
        hp [ XH.mesh [ XP.src $ base <> "assets/res/teapot/teapot.json"
                     , XP.transform "#pitchTransform"
                     ] 
             []
           ]
      , XH.group [ XP.transform "#planeTransform" ] $
        hp [ XH.mesh [ XP.src $ base <> "externalXml/resource/plane.xml#mesh1"
                     , XP.material $ base <> "externalXml/resource/plane.xml#shader_matte"
                     ]
             []
           ]
      , XH.group [ XP.transform "#cubeTransform"
                 , XP.material "#videoMat"
                 ] $
        hp [ XH.mesh [ XP.src $ base <> "assets/res/cube/cube.xml#mesh" ]
             []
           ]
      , XH.group [ XP.transform "#engineerTransform" ] $
        hp [ XH.model [ XP.src $ base <> "assets/res/assets.xml#engineer" ]
             []
           ]
      ]
    ]
    where fx = toNumber x
          fy = toNumber y
          r = fx / 512.0
          g = fy / 512.0
          b = r + g
          col = Vec3 r g b
          base = "http://xml3d.github.io/xml3d-examples/examples/"
          hp m = [ XH.group [ XP.transform "#headingTransform" ]
                   [ XH.group [ XP.transform "#pitchTransform" ] m ] ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Move e next -> do
      H.put $ State (clientX e) (clientY e)
      pure next
    Drawn e next -> do
      pure next
