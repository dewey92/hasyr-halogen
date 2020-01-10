module Hasyr.Task.TaskList where

import Prelude

import Data.Array (null)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen (Component, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as H
import Hasyr.Task.Apis (class ManageTasks, getAllTasks)
import Hasyr.Task.Types (Tasks)
import Network.RemoteData (RemoteData(..), fromEither)

type State =
  { tasks :: RemoteData String Tasks }

data Action = Init | FetchTasks

component :: ∀ m. ManageTasks m => Component H.HTML (Const Void) {} {} m
component = mkComponent
  { initialState
  , eval: mkEval $ defaultEval
    { initialize = Just Init
    , handleAction = handleAction
    }
  , render
  } where

  initialState _ = { tasks: NotAsked }

  handleAction Init = do
    handleAction FetchTasks

  handleAction FetchTasks = do
    modify_ (_ { tasks = Loading })
    tasks <- getAllTasks
    modify_ (_ { tasks = fromEither tasks })

  render state =
    H.section_ [
      renderItem state.tasks
    ]

renderItem :: ∀ p a. RemoteData String Tasks -> H.HTML p a
renderItem = case _ of
  NotAsked   -> H.text "No items yet"
  Loading    -> H.text "Loading..."
  Failure e  -> H.text $ "Oops, an error occurred: " <> e
  Success t
    | null t -> H.text "No tasks yet"
  Success ts ->
    H.ul_ $
      ts <#> \task -> H.li_ [ H.text task.name ]