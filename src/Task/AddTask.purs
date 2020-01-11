module Hasyr.Task.AddTask where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (errorShow)
import Halogen (Component, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Hasyr.AppM (AppM)
import Hasyr.Task.Apis (addTask)
import Hasyr.Task.Types (Task)
import Hasyr.Utils.HTML (className, (<:>))
import Network.RemoteData (RemoteData(..), isLoading, isSuccess)
import Network.RemoteData as RD
import Partial.Unsafe (unsafePartial)
import Web.UIEvent.KeyboardEvent as KE

type State = { inputValue :: String, addTaskRD :: RD.RemoteData String Task }

data Action
  = UpdateInputAdd String
  | DetectKeyUpAdd KE.KeyboardEvent
  | ResetInput

data Output = NewTaskAdded Task

component :: Component H.HTML (Const Void) {} Output AppM
component = mkComponent
  { initialState: const { inputValue: "", addTaskRD: NotAsked }
  , eval: mkEval $ defaultEval { handleAction = handleAction }
  , render
  } where

  handleAction (UpdateInputAdd val) = modify_ _{ inputValue = val }
  handleAction (DetectKeyUpAdd e) = when (KE.key e == "Enter") do
    modify_ _{ addTaskRD = Loading }
    { inputValue: newTaskName } <- get
    newTaskRD <- RD.fromEither <$> addTask newTaskName
    modify_ _{ addTaskRD = newTaskRD }
    if isSuccess newTaskRD
    then do
      let newTask = unsafePartial $ fromJust $ RD.toMaybe newTaskRD
      handleAction ResetInput
      raise $ NewTaskAdded newTask
      liftAff $ delay (Milliseconds 2000.0)
      modify_ _{ addTaskRD = NotAsked }
    else do
      errorShow "Error!!"
  handleAction ResetInput = modify_ _{ inputValue = "" }

  render { inputValue, addTaskRD } =
    H.section_ [
      H.div [className $ "control has-icons-right" <:> cxControl addTaskRD] [
        H.input [
          className $ "input" <:> cxInput addTaskRD,
          P.value inputValue,
          P.placeholder "What needs to be done",
          P.autofocus true,
          P.disabled (isLoading addTaskRD),
          E.onValueChange (Just <<< UpdateInputAdd),
          E.onKeyUp (Just <<< DetectKeyUpAdd)
        ],
        if isSuccess addTaskRD
        then H.span [className "icon is-small is-right"] [H.i [className "fas fa-check"] []]
        else H.text ""
      ]
    ] where
    cxInput (Success s) = "is-success"
    cxInput (Failure s) = "is-danger"
    cxInput _ = ""

    cxControl (Loading) = "is-loading"
    cxControl _ = ""
