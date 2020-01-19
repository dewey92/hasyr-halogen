module Hasyr.Components.Card where

import Halogen.HTML as H
import Hasyr.Utils.HTML (className)

-- Necessary class to be set on action button/links
type ActionClass = String

type Props action slots m =
  { content :: H.ComponentHTML action slots m
  , actions :: ActionClass -> Array (H.ComponentHTML action slots m)
  }

block :: ∀ action slots m. Props action slots m -> H.ComponentHTML action slots m
block props =
  H.div [className "card"] [
    H.div [className "card-content"] [
      H.div [className "content"] [
        props.content
      ]
    ],
    H.footer [className "card-footer"] (props.actions "card-footer-item")
  ]
