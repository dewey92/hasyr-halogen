module Hasyr.Task.Apis
  ( class ManageTasks
  , getAllTasks
  , addTask
  , updateTaskName
  , deleteTask
  ) where

import Prelude

import Data.Bifunctor (rmap)
import Data.Either (Either)
import Effect.Aff (Aff)
import Halogen (HalogenM, lift, liftAff)
import Hasyr.AppM (AppM)
import Hasyr.Task.Types (Task, TaskId, Tasks, decodeTask, decodeTasks)
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

baseFakeUrl :: String
baseFakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks"

fakeServerGetTasks :: Aff (Either String Tasks)
fakeServerGetTasks = do
  tasksJson <- ajaxGet baseFakeUrl
  pure $ tasksJson >>= decodeTasks

fakeServerAddTask :: String -> Aff (Either String Task)
fakeServerAddTask taskName = do
  taskJson <- ajaxPost baseFakeUrl { name: taskName }
  pure $ taskJson >>= decodeTask

fakeServerUpdateTaskName :: TaskId -> String -> Aff (Either String Task)
fakeServerUpdateTaskName taskId taskName = do
  let fakeUrl = baseFakeUrl<> show taskId
  taskJson <- ajaxPatch fakeUrl { name: taskName }
  pure $ taskJson >>= decodeTask

fakeServerDeleteTask :: TaskId -> Aff (Either String Unit)
fakeServerDeleteTask taskId = do
  let fakeUrl = baseFakeUrl <> show taskId
  _del <- ajaxDelete fakeUrl
  pure $ rmap (const unit) _del
