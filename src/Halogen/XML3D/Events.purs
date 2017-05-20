module Halogen.XML3D.Events where

import Prelude
import DOM.Event.Types (Event, EventType(..))
import DOM.XML3D.Event.Types (FrameDrawnEvent)
import Data.Maybe (Maybe)
import Halogen.HTML.Events (handler)
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)

frameDrawnHandler :: forall i. (FrameDrawnEvent -> Maybe i) -> Event -> Maybe i
frameDrawnHandler = unsafeCoerce

onFrameDrawn :: forall r i. (FrameDrawnEvent -> Maybe i) -> IProp (onFrameDrawn :: FrameDrawnEvent | r) i
onFrameDrawn = handler (EventType "framedrawn") <<< frameDrawnHandler
