module Hasyr.Task.TaskList ( component ) where

import Prelude

import Data.Array (filter, null)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (Component, defaultEval, get, mkComponent, mkEval, modify_, put)
import Halogen.HTML as H
import Hasyr.AppM (AppM)
import Hasyr.Task.AddTask as AddTask
import Hasyr.Task.Apis (getAllTasks)
import Hasyr.Task.TaskItem as TaskItem
import Hasyr.Task.Types (Task, Tasks, TaskId)
import Hasyr.Utils.HTML (whenElem)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD

type State =
  { tasksRD :: RemoteData String Tasks
  , tasks :: Tasks
  , selectedTaskIds :: Array TaskId
  }

data Action
  = Init
  | FetchTasks
  | AddTask Task
  | EditTask Task
  | DeleteTask TaskId
  | SelectTask TaskId
  | DeselectTask TaskId

component :: Component H.HTML (Const Void) {} {} AppM
component = mkComponent
  { initialState: const { tasksRD: NotAsked, tasks: [], selectedTaskIds: [] }
  , eval: mkEval $ defaultEval
    { initialize = Just Init
    , handleAction = handleAction
    }
  , render
  } where

  handleAction = case _ of
    Init -> handleAction FetchTasks
    FetchTasks -> do
      modify_ _{ tasksRD = Loading }
      tasksRD <- RD.fromEither <$> getAllTasks
      st <- get
      let mergedTasks = RD.maybe st.tasks (st.tasks <> _) tasksRD
      put $ st { tasksRD = tasksRD, tasks = mergedTasks }
    AddTask task -> modify_ (\st -> st { tasks = st.tasks <> [task] })
    EditTask task -> do
      { tasks } <- get
      let newTasks = tasks <#> \t -> if t.id == task.id then task else t
      modify_ _{ tasks = newTasks }
    DeleteTask taskId -> do
      { tasks } <- get
      let newTasks = filter (_.id >>> notEq taskId) tasks
      modify_ _{ tasks = newTasks }
    SelectTask taskId -> modify_ \st -> st { selectedTaskIds = st.selectedTaskIds <> [taskId] }
    DeselectTask taskId -> modify_ \st -> st { selectedTaskIds = filter (_ /= taskId) st.selectedTaskIds }

  render state@{ selectedTaskIds } =
    H.section_ [
      H.slot _addTask unit AddTask.component {} handleAddTaskOutput,
      whenElem (not $ null selectedTaskIds) \_ ->
        H.text $ "Selected item(s): " <> show selectedTaskIds,
      renderItem state
    ]

  renderItem { tasksRD, tasks } = case tasksRD of
    NotAsked   -> H.text "No items yet"
    Loading    -> H.text "Loading..."
    Failure e  -> H.text $ "Oops, an error occurred: " <> e
    Success t
      | null t -> H.text "No tasks yet"
    Success _ ->
      H.ul_ $
        tasks <#> \task -> H.slot _taskItem task.id TaskItem.component task handleTaskItemOutput

_addTask = SProxy :: SProxy "addTask"
_taskItem = SProxy :: SProxy "taskItem"

handleAddTaskOutput :: AddTask.Output -> Maybe Action
handleAddTaskOutput (AddTask.NewTaskAdded task) = Just $ AddTask task

handleTaskItemOutput :: TaskItem.Output -> Maybe Action
handleTaskItemOutput = case _ of
  TaskItem.TaskEdited task -> Just $ EditTask task
  TaskItem.TaskDeleted taskId -> Just $ DeleteTask taskId
  TaskItem.TaskSelectToggled taskId isSelected -> case isSelected of
    true -> Just $ SelectTask taskId
    false -> Just $ DeselectTask taskId
