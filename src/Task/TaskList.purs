module Hasyr.Task.TaskList where

import Prelude

import Data.Array (null)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (Component, defaultEval, get, mkComponent, mkEval, modify_, put)
import Halogen.HTML as H
import Hasyr.AppM (AppM)
import Hasyr.Task.AddTask as AddTask
import Hasyr.Task.Apis (getAllTasks)
import Hasyr.Task.Types (Tasks, Task)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD

type State =
  { tasksRD :: RemoteData String Tasks
  , tasks :: Tasks
  }

data Action = Init | FetchTasks | AddTask Task

component :: Component H.HTML (Const Void) {} {} AppM
component = mkComponent
  { initialState
  , eval: mkEval $ defaultEval
    { initialize = Just Init
    , handleAction = handleAction
    }
  , render
  } where

  initialState _ = { tasksRD: NotAsked, tasks: [] }

  handleAction Init = do
    handleAction FetchTasks
  handleAction FetchTasks = do
    modify_ (_ { tasksRD = Loading })
    tasksRD <- RD.fromEither <$> getAllTasks
    st <- get
    let mergedTasks = RD.maybe st.tasks (\tasks' -> st.tasks <> tasks') tasksRD
    put $ st { tasksRD = tasksRD, tasks = mergedTasks }
  handleAction (AddTask task) = do
    st <- get
    put $ st { tasks = st.tasks <> [task] }

  render state =
    H.section_ [
      H.slot _addTask unit AddTask.component {} onNewTaskAdded,
      renderItem state
    ]

_addTask = SProxy :: _ "addTask"

onNewTaskAdded :: AddTask.Output -> Maybe Action
onNewTaskAdded (AddTask.NewTaskAdded task) = Just $ AddTask task
onNewTaskAdded _ = Nothing

renderItem :: ∀ p a. State -> H.HTML p a
renderItem { tasksRD, tasks } = case tasksRD of
  NotAsked   -> H.text "No items yet"
  Loading    -> H.text "Loading..."
  Failure e  -> H.text $ "Oops, an error occurred: " <> e
  Success t
    | null t -> H.text "No tasks yet"
  Success _ ->
    H.ul_ $
      tasks <#> \task -> H.li_ [ H.text task.name ]
