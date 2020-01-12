module Hasyr.Task.Apis
  ( class ManageTasks
  , getAllTasks
  , addTask
  , updateTaskName
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson)
import Data.Either (Either)
import Effect.Aff (Aff)
import Halogen (HalogenM, lift, liftAff)
import Hasyr.AppM (AppM)
import Hasyr.Task.Types (Task, Tasks, TaskId)
import Hasyr.Utils.HTTP (ajaxGet, ajaxPatch, ajaxPost)

class Monad m <= ManageTasks m where
  getAllTasks :: m (Either String Tasks)
  addTask :: String -> m (Either String Task)
  updateTaskName :: TaskId -> String -> m (Either String Task)

instance manageTasksAppM :: ManageTasks AppM where
  getAllTasks = liftAff fakeServerGetTasks
  addTask = liftAff <<< fakeServerAddTask
  updateTaskName id name = liftAff $ fakeServerUpdateTaskName id name

instance manageTasksHalogenM :: ManageTasks m => ManageTasks (HalogenM st act cs msg m) where
  getAllTasks = lift getAllTasks
  addTask = lift <<< addTask
  updateTaskName id name = lift $ updateTaskName id name

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
