module Hasyr.Task.Apis
  ( class ManageTasks
  , getAllTasks
  , addTask
  , updateTaskName
  , deleteTask
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Effect.Aff (Aff)
import Halogen (HalogenM, lift, liftAff)
import Hasyr.AppM (AppM)
import Hasyr.Task.Types (Task, Tasks, TaskId)
import Hasyr.Utils.HTTP (ajaxDelete, ajaxGet, ajaxPatch, ajaxPost)

class Monad m <= ManageTasks m where
  getAllTasks :: m (Either String Tasks)
  addTask :: String -> m (Either String Task)
  updateTaskName :: TaskId -> String -> m (Either String Task)
  deleteTask :: TaskId -> m (Either String Unit)

instance manageTasksAppM :: ManageTasks AppM where
  getAllTasks = liftAff fakeServerGetTasks
  addTask = liftAff <<< fakeServerAddTask
  updateTaskName id name = liftAff $ fakeServerUpdateTaskName id name
  deleteTask = liftAff <<< fakeServerDeleteTask

instance manageTasksHalogenM :: ManageTasks m => ManageTasks (HalogenM st act cs msg m) where
  getAllTasks = lift getAllTasks
  addTask = lift <<< addTask
  updateTaskName id name = lift $ updateTaskName id name
  deleteTask = lift <<< deleteTask

taskFromJson :: Json -> Either String Task
taskFromJson = decodeJson

tasksFromJson :: Json -> Either String Tasks
tasksFromJson = decodeJson

fakeServerGetTasks :: Aff (Either String Tasks)
fakeServerGetTasks = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks"
  tasksJson <- ajaxGet fakeUrl
  pure $ tasksJson >>= tasksFromJson

fakeServerAddTask :: String -> Aff (Either String Task)
fakeServerAddTask taskName = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks"
  taskJson <- ajaxPost fakeUrl { name: taskName }
  pure $ taskJson >>= taskFromJson

fakeServerUpdateTaskName :: TaskId -> String -> Aff (Either String Task)
fakeServerUpdateTaskName taskId taskName = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks/" <> show taskId
  taskJson <- ajaxPatch fakeUrl { name: taskName }
  pure $ taskJson >>= taskFromJson

fakeServerDeleteTask :: TaskId -> Aff (Either String Unit)
fakeServerDeleteTask taskId = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks/" <> show taskId
  _del <- ajaxDelete fakeUrl
  pure $ rmap (const unit) _del
