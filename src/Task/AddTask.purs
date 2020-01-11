module Hasyr.Task.AddTask where

import Prelude

import Data.Const (Const)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect)
import Effect.Now (nowDateTime)
import Halogen (Component, defaultEval, get, liftEffect, mkComponent, mkEval, put, raise)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Hasyr.Task.Types (Task)
import Hasyr.Utils.HTML (className)
import Web.UIEvent.KeyboardEvent as KE

type State = String -- the input value

data Action
  = UpdateInputAdd String
  | DetectKeyUpAdd KE.KeyboardEvent
  | ResetInput

data Output = NewTaskAdded Task

component :: ∀ m. MonadEffect m => Component H.HTML (Const Void) {} Output m
component = mkComponent
  { initialState: const ""
  , eval: mkEval $ defaultEval { handleAction = handleAction }
  , render
  } where

  handleAction (UpdateInputAdd val) = put val
  handleAction (DetectKeyUpAdd e) = when (KE.key e == "Enter") do
    newTask <- get
    (Milliseconds ms) <- liftEffect $ nowDateTime <#> (fromDateTime >>> unInstant)
    handleAction ResetInput
    raise $ NewTaskAdded { id: ms, name: newTask }
  handleAction ResetInput = put ""

  render inputValue =
    H.section_ [
      H.input [
        className "input",
        P.value inputValue,
        P.placeholder "What needs to be done",
        P.autofocus true,
        E.onValueChange (Just <<< UpdateInputAdd),
        E.onKeyUp (Just <<< DetectKeyUpAdd)
      ]
    ]