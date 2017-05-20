module Main where

import Prelude
import Example as E
import Halogen.Aff as HA
import Head as H
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Traversable (traverse_)
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  traverse_ (runUI H.head unit) =<< HA.selectElement (QuerySelector "head")
  traverse_ (runUI E.example unit) =<< HA.selectElement (QuerySelector "body")

