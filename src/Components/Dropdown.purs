module Hasyr.Components.Dropdown where

import Halogen.HTML as H
import Hasyr.Utils.HTML (className)

type Props w i =
  { trigger :: H.HTML w i
  , content :: H.HTML w i
  }

block :: ∀ w i. Props w i -> H.HTML w i
block props =
  H.div [className "dropdown is-hoverable is-right"] [
    H.div [className "dropdown-trigger"] [
      props.trigger
    ],
    H.div [className "dropdown-menu"] [
      H.div [className "dropdown-content"] [
        H.div [className "dropdown-item"] [
          props.content
        ]
      ]
    ]
  ]
