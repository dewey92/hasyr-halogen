module Hasyr.Components.Dropdown where

import Prelude

import Data.Symbol (SProxy(..))
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML as H
import Hasyr.Utils.HTML (className)

type Props ca cs m =
  { dropdownTrigger :: H.ComponentHTML ca cs m
  , dropdownContent :: H.ComponentHTML ca cs m
  }

component :: ∀ q ca cs m. Component H.HTML q (Props ca cs m) {} m
component = mkComponent
  { initialState: identity
  , eval: mkEval defaultEval
  , render
  } where

  render state =
    H.div [className "dropdown is-hoverable"] [
      H.div [className "dropdown-trigger"] [
        state.dropdownTrigger
      ],
      H.div [className "dropdown-menu"] [
        H.div [className "dropdown-content"] [
          H.div [className "dropdown-item"] [
            state.dropdownContent
          ]
        ]
      ]
    ]

_dt = SProxy :: SProxy "dropdownTrigger"
_dc = SProxy :: SProxy "dropdownContent"