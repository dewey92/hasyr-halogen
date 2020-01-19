module Hasyr.Task.AddTask ( Output(..), component ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (errorShow)
import Halogen (Component, defaultEval, mkComponent, mkEval, modify_, query, raise, tell)
import Halogen.HTML as H
import Hasyr.Components.AsyncInput as AsyncInput
import Hasyr.Task.Apis (class ManageTasks, addTask)
import Hasyr.Task.Types (Task)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD

type State = { addTaskRD :: RD.RemoteData String Task }

data Action = AddNewTask String

data Output = NewTaskAdded Task

component :: ∀ m.
  MonadEffect m =>
  ManageTasks m =>
  Component H.HTML (Const Void) {} Output m
component = mkComponent
  { initialState: const { addTaskRD: NotAsked }
  , eval: mkEval $ defaultEval { handleAction = handleAction }
  , render
  } where

  handleAction (AddNewTask newTaskName) = do
    modify_ _{ addTaskRD = Loading }
    newTaskRD <- RD.fromEither <$> addTask newTaskName
    modify_ _{ addTaskRD = newTaskRD }
    case newTaskRD of
      Success newTask -> do
        void $ query _asyncInput unit $ tell AsyncInput.ResetInput
        raise $ NewTaskAdded newTask
        modify_ _{ addTaskRD = NotAsked }
      Failure e -> errorShow $ "Error: " <> e
      _ -> pure unit

  render { addTaskRD } =
    H.section_ [
      H.slot _asyncInput unit AsyncInput.component asyncInputProps handleAsyncInputOutput
    ] where
    asyncInputProps = {
      name: "add-new-task",
      placeholder: "What needs to be done?",
      initialValue: "",
      asyncStatus: addTaskRD
    }

_asyncInput = SProxy :: SProxy "asyncInput"

handleAsyncInputOutput :: AsyncInput.Output -> Maybe Action
handleAsyncInputOutput = case _ of
  AsyncInput.EnterPressed val -> Just $ AddNewTask val
  _ -> Nothing
