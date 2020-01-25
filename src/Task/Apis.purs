module Hasyr.Task.Apis
  ( class ManageTasks
  , getAllTasks
  , addTask
  , updateTask
  , deleteTask
  ) where

import Prelude

import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Halogen (HalogenM, lift, liftAff)
import Hasyr.AppM (AppM)
import Hasyr.Task.Types (DateTime(..), Task, TaskId, Tasks, decodeTask, decodeTasks)
import Hasyr.Utils.HTTP (ajaxDelete, ajaxGet, ajaxPatch, ajaxPost)
import Record as Record

class Monad m <= ManageTasks m where
  getAllTasks :: m (Either String Tasks)
  addTask :: String -> m (Either String Task)
  updateTask :: Task -> m (Either String Task)
  deleteTask :: TaskId -> m (Either String Unit)

instance manageTasksAppM :: ManageTasks AppM where
  getAllTasks = liftAff fakeServerGetTasks
  addTask = liftAff <<< fakeServerAddTask
  updateTask = liftAff <<< fakeServerUpdateTask
  deleteTask = liftAff <<< fakeServerDeleteTask

instance manageTasksHalogenM :: ManageTasks m => ManageTasks (HalogenM st act cs msg m) where
  getAllTasks = lift getAllTasks
  addTask = lift <<< addTask
  updateTask = lift <<< updateTask
  deleteTask = lift <<< deleteTask

baseFakeUrl :: String
baseFakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks/"

fakeServerGetTasks :: Aff (Either String Tasks)
fakeServerGetTasks = do
  tasksJson <- ajaxGet baseFakeUrl
  pure $ tasksJson >>= decodeTasks

fakeServerAddTask :: String -> Aff (Either String Task)
fakeServerAddTask taskName = do
  nowDt <- liftEffect nowDateTime
  let fakeNewTask = {
    id: 0,
    name: taskName,
    meta: { note: Nothing, dueDate: Nothing },
    createdAt: DateTime nowDt
  }
  let toSend = Record.delete (SProxy :: _ "id") (fakeNewTask :: Task)
  taskJson <- ajaxPost baseFakeUrl toSend
  pure $ taskJson >>= decodeTask

fakeServerUpdateTask :: Task -> Aff (Either String Task)
fakeServerUpdateTask newTask = do
  let fakeUrl = baseFakeUrl <> show newTask.id
  taskJson <- ajaxPatch fakeUrl newTask
  pure $ taskJson >>= decodeTask

fakeServerDeleteTask :: TaskId -> Aff (Either String Unit)
fakeServerDeleteTask taskId = do
  let fakeUrl = baseFakeUrl <> show taskId
  _del <- ajaxDelete fakeUrl
  pure $ rmap (const unit) _del
