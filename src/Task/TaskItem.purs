module Hasyr.Task.TaskItem ( Output(..), component ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (Component, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Hasyr.AppM (AppM)
import Hasyr.Components.AsyncInput as AsyncInput
import Hasyr.Task.Apis (updateTaskName)
import Hasyr.Task.Types (Task)
import Hasyr.Utils.HTML (className, onClick_)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD

type State =
  { isEditing :: Boolean
  , inputValue :: String
  , taskRD :: RemoteData String Task
  }

data Action
  = StartEditing
  | CancelEditing
  | UpdateTaskName String
  | DeleteTask
  | ReplaceTaskFromParent Task

data Output = TaskEdited Task

component :: Component H.HTML (Const Void) Task Output AppM
component = mkComponent
  { initialState: \input ->
    { isEditing: false
    , task: input
    , taskRD: NotAsked
    }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , receive = handleReceive }
  , render
  } where

  handleReceive = Just <<< ReplaceTaskFromParent

  handleAction = case _ of
    StartEditing -> modify_ _{ isEditing = true }
    UpdateTaskName newName -> do
      modify_ _{ taskRD = Loading }
      { task } <- get
      updateTaskRD <- RD.fromEither <$> updateTaskName task.id newName
      modify_ _{ taskRD = updateTaskRD }
      case updateTaskRD of
        Success newlyEditedTask -> do
          handleAction CancelEditing
          raise $ TaskEdited newlyEditedTask
        _ -> pure unit
    CancelEditing -> modify_ _{ isEditing = false }
    ReplaceTaskFromParent task -> modify_ _{ task = task }
    _ -> pure unit

  render { task, isEditing, taskRD } =
    H.li_ [
      H.div [className "card has-margin-top-20"] [
        H.div [className "card-content"] [
          if isEditing
          then H.slot _asyncInput unit AsyncInput.component asyncInputProps handleAsyncInputOutput
          else H.p [className "title"] [H.text task.name]
        ],
        H.footer [className "card-footer"] [
          H.a [onClick_ \_ -> Just StartEditing, className "card-footer-item", P.href "#"] [H.text "Edit"],
          H.a [onClick_ \_ -> Just DeleteTask, className "card-footer-item", P.href "#"] [H.text "Delete"]
        ]
      ]
    ] where
    asyncInputProps = {
      name: "edit-task",
      placeholder: "Edit task",
      initialValue: task.name,
      asyncStatus: taskRD
    }

_asyncInput = SProxy :: SProxy "asyncInput"

handleAsyncInputOutput :: AsyncInput.Output -> Maybe Action
handleAsyncInputOutput (AsyncInput.EnterPressed val) = Just $ UpdateTaskName val
