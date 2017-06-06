module Main where

import Prelude
import Example as E
import Halogen.Aff as HA
import Control.Monad.Eff (Eff)
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI E.example unit
