module Hasyr.Task.TaskItem ( Output(..), component ) where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Const (Const)
import Data.Foldable (fold, for_)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Halogen (Component, RefLabel(..), SubscriptionId, defaultEval, get, getHTMLElementRef, mkComponent, mkEval, modify, modify_, raise, subscribe, unsubscribe)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query.EventSource (eventListenerEventSource)
import Hasyr.Components.AsyncInput as AsyncInput
import Hasyr.Components.Modal as Modal
import Hasyr.Task.Apis (class ManageTasks, deleteTask, updateTask)
import Hasyr.Task.TaskDetails as TaskDetails
import Hasyr.Task.Types (Task, TaskId)
import Hasyr.Utils.HTML (className, isClickOutside, onClick_, (<:>))
import Network.RemoteData (RemoteData(..), isLoading)
import Network.RemoteData as RD
import Web.Event.Internal.Types (Event) as ET
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click) as ET
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)

type State =
  { isEditing :: Boolean
  , inputValue :: String
  , taskRD :: RemoteData String Task
  , isChecked :: Boolean
  , showDeleteModal :: Boolean
  , outsideClickId :: Maybe SubscriptionId
  }

data Action
  = AddOutsideClickListener
  | RemoveOutsideClickListener
  | DetectOutsideClick ET.Event
  | StartEditing
  | CancelEditing
  | UpdateTaskName String
  | DeleteTask
  | ShowDeleteModal
  | HideDeleteModal
  | SelectSelf
  | Receive Task

data Output
  = TaskEdited Task
  | TaskDeleted TaskId
  | TaskSelectToggled TaskId Boolean

component :: ∀ m. MonadAff m => ManageTasks m => Component H.HTML (Const Void) Task Output m
component = mkComponent
  { initialState: \input ->
    { isEditing: false
    , task: input
    , taskRD: NotAsked
    , isChecked: false
    , showDeleteModal: false
    , outsideClickId: Nothing
    }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , receive = Just <<< Receive
    }
  , render
  } where

  handleAction = case _ of
    AddOutsideClickListener -> do
      doc <- liftEffect $ window >>= document <#> toEventTarget
      sId <- subscribe $ eventListenerEventSource (ET.click) doc (\ev -> Just $ DetectOutsideClick ev)
      modify_ _{ outsideClickId = Just sId }
    DetectOutsideClick ev -> do
      { task, isEditing } <- get
      elemMaybe <- getHTMLElementRef (RefLabel $ "task-item-root-" <> show task.id)
      for_ elemMaybe \elem -> do
        detected <- liftEffect $ isClickOutside ev elem
        liftEffect $ logShow [show detected, show task.id]
        if detected && isEditing
          -- FIXME: currently `detected` is always true, not sure why
          then pure unit -- handleAction CancelEditing
          else pure unit
    RemoveOutsideClickListener -> do
      { outsideClickId } <- get
      for_ outsideClickId unsubscribe
      modify_ _{ outsideClickId = Nothing }
    StartEditing -> do
      modify_ _{ isEditing = true }
      handleAction AddOutsideClickListener
    CancelEditing -> do
      modify_ _{ isEditing = false }
      handleAction RemoveOutsideClickListener
    UpdateTaskName newName -> do
      modify_ _{ taskRD = Loading }
      { task } <- get
      updateTaskRD <- RD.fromEither <$> updateTask (task { name = newName })
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
    ShowDeleteModal -> modify_ _{ showDeleteModal = true }
    HideDeleteModal -> modify_ _{ showDeleteModal = false }
    SelectSelf -> do
      { task, isChecked } <- modify (\st -> st { isChecked = not st.isChecked })
      raise $ TaskSelectToggled task.id isChecked
    Receive task -> modify_ _{ task = task }

  render state@{ task, isEditing, taskRD, showDeleteModal } =
    H.li [P.ref (RefLabel $ "task-item-root-" <> show task.id), className $ "task-item" <:> guard isEditing "is-editing"] [
      H.fieldset [disableWhenLoading] [
        H.div [className "columns is-vcentered is-variable is-2 no-vmargin"] [
          H.div [className "column is-narrow"] [
            H.label [className "checkbox"] [
              H.input [className "checkbox", P.type_ InputCheckbox, E.onChecked \_ -> Just SelectSelf]
            ]
          ],
          H.div [className "column"] [
            renderTaskDetails state
          ]
        ],
        -- Place modal inside fieldset to have buttons disabled automatically
        -- when sending remote data
        Modal.block $ Modal.defaultProps {
          modalShown = showDeleteModal,
          rootClassNames = "delete-item-modal",
          onClose = \_ -> Just HideDeleteModal,
          content =
            H.div [className "flex delete-item-content"] [
              H.div [className "title is-5"] [
                H.text $ "\"" <> task.name <> "\"" <> "will be permanently deleted."
              ],
              H.div [className "subtitle is-6"] [
                H.text "You won't be able to undo this action"
              ],
              H.div [className "buttons justify-flex-end"] [
                H.button [onClick_ \_ -> Just DeleteTask, className "button is-danger is-light is-small"] [
                  H.text "Confirm delete"
                ],
                H.button [onClick_ \_ -> Just HideDeleteModal, className "button is-small"] [
                  H.text "Cancel"
                ]
              ]
            ]
        }
      ]
    ] where
    disableWhenLoading = P.disabled (isLoading taskRD)

  renderTaskDetails { task, isEditing, taskRD } =
    if isEditing
    then
      H.div_ [
        H.slot _asyncInput unit AsyncInput.component (AsyncInput.defaultInput {
          name = "edit-task",
          placeholder = "Edit task",
          initialValue = task.name,
          asyncStatus = taskRD,
          inputClassName = "is-borderless task-item-edit-input"
        }) handleAsyncInputOutput,
        H.slot _taskDetails unit TaskDetails.component {
          taskMeta: task.meta,
          taskRD
        } handleTaskDetailsOutput
      ]
    else
      H.div_ [
        H.p [
          className $ "title is-5 task-name" <:> guard (isLoading taskRD) "has-text-grey-light",
          onClick_ \_ -> Just StartEditing
        ] [
          H.text task.name
        ],
        H.p [className "subtitle is-6 has-text-grey"] [
          H.text $ fold task.meta.note
        ]
      ]

_asyncInput = SProxy :: SProxy "asyncInput"
_taskDetails = SProxy :: SProxy "taskDetails"

handleAsyncInputOutput :: AsyncInput.Output -> Maybe Action
handleAsyncInputOutput = case _ of
  AsyncInput.EnterPressed val -> Just $ UpdateTaskName val
  AsyncInput.EscPressed -> Just CancelEditing
  AsyncInput.InputBlurred -> Nothing

handleTaskDetailsOutput :: TaskDetails.Output -> Maybe Action
handleTaskDetailsOutput = case _ of
  TaskDetails.DeleteButtonClicked -> Just ShowDeleteModal
  _ -> Nothing
