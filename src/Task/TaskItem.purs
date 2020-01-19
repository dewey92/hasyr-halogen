module Hasyr.Task.TaskItem ( Output(..), component ) where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Halogen (Component, defaultEval, get, mkComponent, mkEval, modify, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Hasyr.AppM (AppM)
import Hasyr.Components.AsyncInput as AsyncInput
import Hasyr.Components.Dropdown as Dropdown
import Hasyr.Task.Apis (deleteTask, updateTaskName)
import Hasyr.Task.Types (Task, TaskId)
import Hasyr.Utils.HTML (className, onClick_, (<:>))
import Network.RemoteData (RemoteData(..), isLoading)
import Network.RemoteData as RD

type State =
  { isEditing :: Boolean
  , inputValue :: String
  , taskRD :: RemoteData String Task
  , isChecked :: Boolean
  }

data Action
  = StartEditing
  | CancelEditing
  | UpdateTaskName String
  | DeleteTask
  | SelectSelf
  | Receive Task

data Output
  = TaskEdited Task
  | TaskDeleted TaskId
  | TaskSelectToggled TaskId Boolean

component :: Component H.HTML (Const Void) Task Output AppM
component = mkComponent
  { initialState: \input ->
    { isEditing: false
    , task: input
    , taskRD: NotAsked
    , isChecked: false
    }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , receive = Just <<< Receive
    }
  , render
  } where

  handleAction = case _ of
    StartEditing -> modify_ _{ isEditing = true }
    CancelEditing -> modify_ _{ isEditing = false }
    UpdateTaskName newName -> do
      modify_ _{ taskRD = Loading }
      { task } <- get
      updateTaskRD <- RD.fromEither <$> updateTaskName task.id newName
      modify_ _{ taskRD = updateTaskRD }
      case updateTaskRD of
        Success newlyEditedTask -> do
          handleAction CancelEditing
          raise $ TaskEdited newlyEditedTask
        _ -> pure unit -- TODO: show err message
    DeleteTask -> do
      { task, taskRD } <- get
      -- don't do anything while it's performing some remote data action
      unless (isLoading taskRD) do
        modify_ _{ taskRD = Loading }
        deleteTaskRD <- RD.fromEither <$> deleteTask task.id
        modify_ _{ taskRD = const task <$> deleteTaskRD }
        case deleteTaskRD of
          Success _ -> raise $ TaskDeleted task.id
          _ -> pure unit -- TODO: show err message
    SelectSelf -> do
      { task, isChecked } <- modify (\st -> st { isChecked = not st.isChecked })
      raise $ TaskSelectToggled task.id isChecked
    Receive task -> modify_ _{ task = task }

  render { task, isEditing, taskRD } =
    H.li [className "task-item"] [
      H.fieldset [disableWhenLoading] [
        H.div [className "columns has-margin-top-10"] [
          H.div [className "column is-narrow"] [
            H.label [className "checkbox"] [
              H.input [className "checkbox", P.type_ InputCheckbox, E.onChecked \_ -> Just SelectSelf]
            ]
          ],
          H.div [className "column"] [
            if isEditing
            then H.slot _asyncInput unit AsyncInput.component asyncInputProps handleAsyncInputOutput
            else H.p [className $ "title is-4" <:> guard (isLoading taskRD) "has-text-grey-light"] [H.text task.name]
          ],
          H.div [className "column is-narrow"] [
            H.div [className "field has-addons"] [
              H.p [className "control"] [
                if isEditing
                then
                  H.button [onClick_ \_ -> Just CancelEditing, className "button is-borderless is-text"] [
                    H.span [className "icon"] [
                      H.i [className "ion-md-undo"] []
                    ]
                  ]
                else
                  H.button [onClick_ \_ -> Just StartEditing, className "button is-borderless is-text"] [
                    H.span [className "icon"] [
                      H.i [className "ion-md-create"] []
                    ]
                  ]
              ],
              H.p [className "control"] [
                H.slot _dropdown unit Dropdown.component {
                  dropdownTrigger:
                    H.button [className "button is-borderless is-danger is-inverted"] [
                      H.span [className "icon"] [
                        H.i [className "ion-md-trash"] []
                      ]
                    ],
                  dropdownContent:
                    H.div_ [
                      H.text "Are you sure bro?",
                      H.button [onClick_ \_ -> Just DeleteTask, className "button is-text"] [H.text "Yeah"]
                    ]
                } (const Nothing)
              ]
            ]
          ]
        ]
      ]
    ] where
    asyncInputProps = {
      name: "edit-task",
      placeholder: "Edit task",
      initialValue: task.name,
      asyncStatus: taskRD
    }
    disableWhenLoading = P.disabled (isLoading taskRD)

_asyncInput = SProxy :: SProxy "asyncInput"
_dropdown = SProxy :: SProxy "dropdown"

handleAsyncInputOutput :: AsyncInput.Output -> Maybe Action
handleAsyncInputOutput = case _ of
  AsyncInput.EnterPressed val -> Just $ UpdateTaskName val
  AsyncInput.EscPressed -> Just CancelEditing
