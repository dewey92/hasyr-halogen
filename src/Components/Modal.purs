module Hasyr.Components.Modal where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen.HTML as H
import Hasyr.Utils.HTML (className, onClick_, (<:>))

type Props action slots m =
  { content :: H.ComponentHTML action slots m
  , modalShown :: Boolean
  , onClose :: Unit -> Maybe action
  , rootClassNames :: String
  }

defaultProps :: ∀ action slots m. Props action slots m
defaultProps =
  { content: H.text ""
  , modalShown: false
  , onClose: const Nothing
  , rootClassNames: ""
  }

block :: ∀ action slots m. Props action slots m -> H.ComponentHTML action slots m
block props =
  H.div [className $ "modal" <:> props.rootClassNames <:> guard props.modalShown "is-active"] [
    H.div [className "modal-background"] [],
    H.div [className "modal-content"] [
      props.content
    ],
    H.button [onClick_ props.onClose, className "modal-close is-large"] []
  ]
