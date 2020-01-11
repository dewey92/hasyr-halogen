module Main where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (errorShow)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Hasyr.AppM (runAppM)
import Hasyr.Container as Container
import Hasyr.Env (Env)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML.HTMLElement (fromElement, toParentNode)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    env :: Env
    env = {}

    rootComponent :: H.Component HH.HTML (Const Void) {} {} Aff
    rootComponent = H.hoist (runAppM env) Container.component

  appEl <- liftEffect $ querySelector (QuerySelector "#app") (toParentNode body)
  case appEl >>= fromElement of
    Nothing -> liftEffect $ errorShow "#app is not found"
    Just el -> do
      halogenIO <- runUI rootComponent {} el
      pure unit