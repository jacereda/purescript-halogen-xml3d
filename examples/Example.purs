module Example where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.XML3D.Elements as XH
-- import Halogen.XML3D.Events as XE
import Halogen.XML3D.Properties as XP
import DOM.XML3D.Indexed.Light (LightModel(..))
import DOM.XML3D.Indexed.Material (MatModel(..))
import DOM.XML3D.Indexed.Vec3 (Vec3(..))
import DOM.XML3D.Indexed.View (ViewModel(..))
import Data.Maybe (Maybe(..))

type State = Boolean

data Query a
  = Toggle a

data Message = Toggled Boolean

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
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div []
    [ HH.button [ HE.onClick (HE.input_ Toggle)
                ]
      [ HH.text "press me" ]
    , XH.xml3d [ XP.width 640
               , XP.height 480
--               , XE.onFrameDrawn (HE.input_ Toggle)
               ]
      [ XH.defs []
        [ XH.material [ XP.id "orangePhong"
                      , XP.matModel Phong ]
          [ XH.float3 [ XP.name "diffuseColor" ]
            [ HH.text if state then "1 0.5 0"  else "0 1 0" ]
          , XH.float [ XP.name "ambientIntensity" ]
            [ HH.text "0.5" ]
          ]
        , XH.transform [ XP.id "cameraTransform"
                       , XP.translation $ Vec3 0.0 0.0 200.0
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
      , XH.group [ XP.material "#orangePhong" ]
        [ XH.mesh [XP.src "http://xml3d.github.io/xml3d-examples/examples/assets/res/teapot/teapot.json"]
          []
        ]
      ]
    ]
    
  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
