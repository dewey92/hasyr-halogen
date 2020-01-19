module Hasyr.Container where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML as H
import Hasyr.Task.Apis (class ManageTasks)
import Hasyr.Task.TaskList as TaskList
import Hasyr.Utils.HTML (className)

_taskList = SProxy :: SProxy "taskList"

component :: ∀ m.
  MonadAff m =>
  ManageTasks m =>
  Component H.HTML (Const Void) {} Void m
component = mkComponent
  { initialState: const unit
  , eval: mkEval defaultEval
  , render } where

  render _ =
    H.div [className "root"] [
      H.div [className "columns is-centered"] [
        H.div [className "column is-half"] [
          H.section_ [
            H.slot _taskList unit TaskList.component {} (const Nothing)
          ]
        ]
      ]
    ]