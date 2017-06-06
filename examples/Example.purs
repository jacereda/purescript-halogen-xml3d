module Example where

import Prelude
import CSS as C
import DOM.Event.TouchEvent as T
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.XML3D.Elements as XH
import Halogen.XML3D.Events as XE
import Halogen.XML3D.Properties as XP
import DOM.Event.MouseEvent (MouseEvent, clientX, clientY)
import DOM.XML3D.Event.Types (FrameDrawnEvent, timeStart)
import DOM.XML3D.Indexed.AxisAngle (AxisAngle(..))
import DOM.XML3D.Indexed.Light (LightModel(..))
import DOM.XML3D.Indexed.Material (MatModel(..))
import DOM.XML3D.Indexed.Texture (Filter(..), MagFilter(..), MinFilter(..), Wrap(..), WrapMode(..))
import DOM.XML3D.Indexed.Vec3 (Vec3(..))
import DOM.XML3D.Indexed.View (ViewModel(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Math (remainder)

type State = { mx :: Int
             , my :: Int
             , t :: Number
             }

data Query a = Move MouseEvent a
             | Touch T.TouchEvent a
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
  initialState = { mx: 0
                 , my: 0
                 , t: 0.0
                 }

  render :: State -> H.ComponentHTML Query
  render s = 
    HH.div [ HP.class_ $ HH.ClassName "_xml3d_hideDiv"
           , HE.onMouseMove (HE.input Move)
           , HE.onTouchMove (HE.input Touch)
           ]
    [ XH.xml3d [ HS.style $ do
                    C.backgroundColor $ C.rgba 0 0 255 1.0
                    C.width $ C.pct 100.0
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
          --   -- [ HH.video [ HP.src $ base <> "../resources/videos/nescafe.mp4"
          --   --            , HP.autoplay true
          --   --            , HP.loop true
          --   --            , HP.muted true
          --   --            ] []
          --   -- ]
            []
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
        , XH.transform [ XP.id "ciccioTransform"
                       , XP.translation $ Vec3 50.0 50.0 0.0
                       , XP.scale $ Vec3 1.0 1.0 1.0
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
      , XH.group [ XP.transform "#ciccioTransform" ] $
        hp [ XH.model [ XP.src $ base2 <> "robots/ciccio.xml#asset" ]
             [ XH.assetdata [ XP.name "animation" ]
               [ XH.float [ XP.name "key" ]
                 [ remainder (s.t / 1000.0) 16.875 ]
               ]
             ]
           ]
      , XH.group [ XP.transform "#engineerTransform" ] $
        hp [ XH.model [ XP.src $ base <> "assets/res/assets.xml#engineer" ]
             []
           ]
      ]
    ]
    where fx = toNumber s.mx
          fy = toNumber s.my
          r = fx / 512.0
          g = fy / 512.0
          b = r + g
          col = Vec3 r g b
          base = "http://xml3d.github.io/xml3d-examples/examples/"
          base2 = "http://cdn.rawgit.com/xml3d/xml3d-examples/master/resources/assets/"
          hp m = [ XH.group [ XP.transform "#headingTransform" ]
                   [ XH.group [ XP.transform "#pitchTransform" ] m ] ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Move e next -> do
      move (clientX e) (clientY e)
      pure next
    Touch e next -> do
      case T.item 0 $ T.touches e of
        Nothing -> pure next
        Just t -> do
          move (T.clientX t) (T.clientY t)
          pure next
    Drawn e next -> do
      H.modify (_ { t = timeStart e })
      pure next

    where move mx my = H.modify (_ { mx = mx
                                   , my = my
                                   })
  
