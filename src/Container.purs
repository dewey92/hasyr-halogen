module Hasyr.Container where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML as H
import Hasyr.AppM (AppM)
import Hasyr.Task.TaskList as TaskList
import Hasyr.Utils.HTML (className)

_taskList = SProxy :: SProxy "taskList"

component :: Component H.HTML (Const Void) {} {} AppM
component = mkComponent
  { initialState: const unit
  , eval: mkEval defaultEval
  , render } where

  render _ =
    H.div [className "root"] [
      H.div [className "columns"] [
        H.div [className "column"] [],
        H.div [className "column is-half"] [
          H.section_ [
            H.slot _taskList unit TaskList.component {} (const Nothing)
          ]
        ],
        H.div [className "column"] []
      ]
    ]