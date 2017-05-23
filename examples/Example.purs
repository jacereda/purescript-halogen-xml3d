module Example where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.XML3D.Elements as XH
import Halogen.XML3D.Properties as XP
import DOM.Event.MouseEvent (MouseEvent, clientX, clientY)
import DOM.XML3D.Indexed.Light (LightModel(..))
import DOM.XML3D.Indexed.Material (MatModel(..))
import DOM.XML3D.Indexed.Vec3 (Vec3(..))
import DOM.XML3D.Indexed.AxisAngle (AxisAngle(..))
import DOM.XML3D.Indexed.View (ViewModel(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

data State = State Int Int

data Query a = Move MouseEvent a

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
    [ XH.xml3d [ XP.width 640
               , XP.height 480
               ]
      [ XH.defs []
        [ XH.material [ XP.id "orangePhong"
                      , XP.matModel Phong ]
          [ XH.float3 [ XP.name "diffuseColor" ]
            [ HH.text $ show r <> " " <> show g <> " " <> show b ]
          , XH.float [ XP.name "ambientIntensity" ]
            [ HH.text "0.5" ]
          ]
        , XH.transform [ XP.id "cameraTransform"
                       , XP.translation $ Vec3 0.0 0.0 200.0
                       ]
          []
        , XH.transform [ XP.id "objectTransform"
--                       , XP.rotation $ AxisAngle 1.0 0.0 0.0 (fy / 50.0)
                       , XP.rotation $ AxisAngle 0.0 1.0 0.0 (fx / 50.0)
                       ]
          []
        , XH.light [ XP.lightModel Directional ]
          [ XH.float3 [ XP.name "intensity" ]
            [ HH.text "1 1 1" ]
          ]
        ]
      , XH.view [ XP.transform "#cameraTransform"
                , XP.viewModel Perspective
                ]
        []
      , XH.group [ XP.material "#orangePhong"
                 , XP.transform "#objectTransform"
                 ]
        [ XH.mesh [XP.src "http://xml3d.github.io/xml3d-examples/examples/assets/res/teapot/teapot.json"]
          []
        ]
      ]
    ]
    where fx = toNumber x
          fy = toNumber y
          r = fx / 512.0
          g = fy / 512.0
          b = r + g
    
  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Move e next -> do
      H.put $ State (clientX e) (clientY e)
      pure next
