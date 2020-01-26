module Hasyr.Task.TaskDetails where

import Prelude

import Data.Const (Const)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, defaultEval, mkComponent, mkEval, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Hasyr.Task.Apis (class ManageTasks)
import Hasyr.Task.Types (Task, Meta)
import Hasyr.Utils.HTML (className, onClick_)
import Network.RemoteData (RemoteData)

type State = { input :: Input }

type Input =
  { taskMeta :: Meta
  , taskRD :: RemoteData String Task
  }

data Action
  = UpdateTaskDetails Meta
  | ClickDeleteButton
  | Receive Input

data Output = TaskMetaEdited Task | DeleteButtonClicked

component :: ∀ m. MonadEffect m => ManageTasks m => Component H.HTML (Const Void) Input Output m
component = mkComponent
  { initialState: \input -> { input }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , receive = Just <<< Receive
    }
  , render
  } where

  handleAction = case _ of
    Receive input -> modify_ _{ input = input }
    ClickDeleteButton -> raise DeleteButtonClicked
    _ -> pure unit

  render { input } =
    H.div [className "todo-meta"] [
      H.div_ [
        H.textarea [
          className "textarea task-item-note is-small is-borderless has-text-grey",
          P.value $ fold input.taskMeta.note,
          P.placeholder "Add a note",
          P.rows 1
        ]
      ],
      H.div [className "flex has-margin-top-10"] [
        H.div [className "buttons justify-flex-end"] [
          H.button [
            className "button is-borderless is-danger is-inverted is-small",
            onClick_ \_ -> Just ClickDeleteButton
          ] [
            H.span_ [ H.text "Delete task" ],
            H.span [className "icon"] [
              H.i [className "ion-md-trash"] []
            ]
          ]
        ]
      ]
    ]