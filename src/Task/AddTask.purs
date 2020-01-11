module Hasyr.Task.AddTask where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (errorShow)
import Halogen (Component, defaultEval, mkComponent, mkEval, modify_, query, raise, tell)
import Halogen.HTML as H
import Hasyr.AppM (AppM)
import Hasyr.Components.AsyncInput as AsyncInput
import Hasyr.Task.Apis (addTask)
import Hasyr.Task.Types (Task)
import Network.RemoteData (RemoteData(..), isSuccess)
import Network.RemoteData as RD
import Partial.Unsafe (unsafePartial)

type State = { addTaskRD :: RD.RemoteData String Task }

data Action = AddNewTask String

data Output = NewTaskAdded Task

component :: Component H.HTML (Const Void) {} Output AppM
component = mkComponent
  { initialState: const { addTaskRD: NotAsked }
  , eval: mkEval $ defaultEval { handleAction = handleAction }
  , render
  } where

  handleAction (AddNewTask newTaskName) = do
    modify_ _{ addTaskRD = Loading }
    newTaskRD <- RD.fromEither <$> addTask newTaskName
    modify_ _{ addTaskRD = newTaskRD }
    if isSuccess newTaskRD
    then do
      let newTask = unsafePartial $ fromJust $ RD.toMaybe newTaskRD
      void $ query _asyncInput unit $ tell AsyncInput.ResetInput
      raise $ NewTaskAdded newTask
      liftAff $ delay (Milliseconds 2000.0)
      modify_ _{ addTaskRD = NotAsked }
    else do
      errorShow "Error!!"

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
handleAsyncInputOutput (AsyncInput.EnterPressed val) = Just $ AddNewTask val
