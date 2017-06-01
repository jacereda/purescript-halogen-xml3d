module Head where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

head :: forall m. H.Component HH.HTML (Const Void) Unit Void m
head =
  H.component { initialState: const unit
              , render: const $ HH.script [ HP.src "http://www.xml3d.org/xml3d/script/xml3d.js" ] []
              , eval: absurd <<< unwrap
              , receiver: const Nothing
              }
