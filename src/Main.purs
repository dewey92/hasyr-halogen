module Main where

import Prelude

import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Hasyr.AppM (runAppM)
import Hasyr.Container as Container
import Hasyr.Env (Env)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    env :: Env
    env = {}

    rootComponent :: H.Component HH.HTML (Const Void) {} {} Aff
    rootComponent = H.hoist (runAppM env) Container.component

  halogenIO <- runUI rootComponent {} body
  pure unit