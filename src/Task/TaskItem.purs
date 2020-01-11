module Hasyr.Task.TaskItem where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class.Console (logShow)
import Halogen (Component, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Hasyr.AppM (AppM)
import Hasyr.Task.Types (Task)
import Hasyr.Utils.HTML (className, onClick_)
import Web.UIEvent.KeyboardEvent as KE

type State = { isEditing :: Boolean , inputValue :: String }

data Action
  = StartEditing
  | CancelEditing
  | UpdateInputEdit String
  | DetectKeyUpEdit KE.KeyboardEvent
  | SaveNewTaskName String
  | DeleteTask
  | ReplaceTaskFromParent Task

data Output = TaskEdited Task

component :: Component H.HTML (Const Void) Task Output AppM
component = mkComponent
  { initialState: \input -> { isEditing: false, inputValue: input.name, task: input }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , receive = handleReceive }
  , render
  } where

  handleReceive = Just <<< ReplaceTaskFromParent

  handleAction StartEditing = modify_ (_ { isEditing = true })
  handleAction (UpdateInputEdit val) = modify_ (_ { inputValue = val })
  handleAction (DetectKeyUpEdit e)
    | KE.key e == "Esc" = handleAction CancelEditing
    | KE.key e == "Enter" = do
      { inputValue } <- get
      handleAction (SaveNewTaskName inputValue)
  handleAction (SaveNewTaskName newName) = do
    { task } <- get
    logShow $ "Saving " <> newName -- TODO: Call endpoint!
    handleAction CancelEditing
    raise $ TaskEdited { id: task.id, name: newName }
  handleAction CancelEditing = modify_ (_ { isEditing = false })
  handleAction (ReplaceTaskFromParent task) = modify_ (_ { task = task })
  handleAction _ = pure unit -- TODO: Delete task!

  render { task, isEditing, inputValue } =
    H.li_ [
      H.div [className "card has-margin-top-20"] [
        H.div [className "card-content"] [
          if isEditing
            then H.input [
                P.value inputValue,
                E.onValueChange (Just <<< UpdateInputEdit),
                E.onKeyUp (Just <<< DetectKeyUpEdit),
                className "input",
                P.placeholder "Edit task name",
                P.autofocus true
              ]
            else H.p [className "title"] [H.text task.name]
        ]
      ],
      H.footer [className "card-footer"] [
        H.a [onClick_ \_ -> Just StartEditing, className "card-footer-item", P.href "#"] [H.text "Edit"],
        H.a [onClick_ \_ -> Just DeleteTask, className "card-footer-item", P.href "#"] [H.text "Delete"]
      ]
    ]
